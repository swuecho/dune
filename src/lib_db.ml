open Import
open Jbuild_types

type t =
  { findlib                  : Findlib.t
  ; (* This include both libraries from the current workspace and external ones *)
    by_public_name           : (string, Lib.t) Hashtbl.t
  ; (* This is to implement the scoping described in the manual *)
    by_internal_name         : (Path.t, Lib.Internal.t String_map.t ref) Hashtbl.t
  ; (* This is to filter out libraries that are not installable because of missing
       dependencies *)
    instalable_internal_libs : Lib.internal String_map.t
  }

let rec internal_name_scope table ~dir =
  match Hashtbl.find table dir with
  | Some scope -> scope
  | None ->
    let scope = internal_name_scope table ~dir:(Path.parent dir) in
    Hashtbl.add table ~key:dir ~data:scope;
    scope

let find_by_internal_name t ~from name =
  let scope = internal_name_scope t.by_internal_name ~dir:from in
  String_map.find name !scope

let find t ~from name =
  match find_by_internal_name t ~from name with
  | Some x -> Lib.Internal x
  | None ->
    Hashtbl.find_or_add t.by_public_name name
      ~f:(fun name ->
        External (Findlib.find_exn t.findlib name))

let find_internal t ~from name =
  match find_by_internal_name t ~from name with
  | Some _ as some -> some
  | None ->
    match Hashtbl.find t.by_public_name name with
    | Some (Internal x) -> Some x
    | _ -> None

module Step1 = struct
  type t = Path.t * Library.t

  type graph =
    { by_internal_name : (Path.t, t String_map.t ref) Hashtbl.t
    ; by_public_name   : t String_map.t
    }

  let create_db ~dirs_with_dot_opam_files internal_libraries =
    let by_internal_name = Hashtbl.create 1024 in
    Path.Set.iter dirs_with_dot_opam_files ~f:(fun dir ->
      Hashtbl.add t.by_internal_name ~key:dir
        ~data:(ref String_map.empty));
    let by_public_name =
      List.fold_left internal_libraries ~init:String_map.empty
        ~f:(fun acc ((dir, lib) as x) ->
          let scope = internal_name_scope by_public_name ~dir in
          scope := String_map.add !scope ~key:lib.Library.name ~data:x;
          match lib.public with
          | None -> acc
          | Some p -> String_map.add t.by_public_name ~key:p.name ~data:x)
    in
    { by_internal_name
    ; by_public_name
    }

  let key ((_, lib) : t) =
    match lib.public with
    | None -> lib.name
    | Some p -> p.name

  let find_internal db ~from name =
    let scope = internal_name_scope db.by_internal_name ~dir:from in
    match String_map.find name !scope with
    | Some _ as some -> some
    | None -> String_map.find name t.by_internal_name

  let deps ((dir, lib) : t) db =
    List.fold_left
      [ List.map lib.virtual_deps ~f:(fun s -> Lib_dep.Direct s)
      ; lib.buildable.libraries
      ; lib.ppx_runtime_deps
      ]
      ~init:[]
      ~f:Lib_dep.add_lib_names
    |> List.fold_left ~init:[] ~f:(fun acc libname ->
      match find_internal db ~from:dir libname with
      | None -> acc
      | Some x -> x :: acc)
end

module Local_closure = Top_closure.Make(String)(Step1)

let top_sort_internals t ~internal_libraries =
  match Local_closure.top_closure t internal_libraries with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle between libraries:\n   %s"
      (List.map cycle ~f:(fun lib -> Lib.describe (Internal lib))
       |> String.concat ~sep:"\n-> ")

let lib_is_installable t ~from name =
  match find_internal t ~from name with
  | Some _ -> String_map.mem lib.name t.instalable_internal_libs
  | None -> Findlib.available t.findlib name

let choice_is_possible t ~from { Lib_dep. lits; _ } =
  List.for_all lits ~f:(function
    | Lib_dep.Pos name ->      lib_is_installable t ~from name
    | Lib_dep.Neg name -> not (lib_is_installable t ~from name))

let resolve_selects t ~from lib_deps =
  List.filter_map lib_deps ~f:(function
    | Lib_dep.Direct _ -> None
    | Select { result_fn; choices } ->
      let src_fn =
        match List.find choices ~f:(choice_is_possible t ~from) with
        | Some c -> c.file
        | None -> "no solution found"
      in
      Some { dst_fn = result_fn; src_fn })

let interpret_lib_deps t ~dir lib_deps =
  let libs, failures =
    List.partition_map lib_deps ~f:(function
      | Lib_dep.Direct name -> begin
          match find t ~from:dir name with
          | x -> Inl [x]
          | exception e ->
            (* Call [find] again to get a proper backtrace *)
            Inr { fail = fun () -> ignore (find t ~from:dir name : Lib.t); raise e }
        end
      | Select { result_fn; choices } ->
        match
          List.find_map choices ~f:(fun { lits; _ } ->
            match
              List.filter_map lits ~f:(function
                | Pos s -> Some (find t ~from:dir s)
                | Neg s ->
                  if lib_is_installable t ~from:dir s then
                    raise Exit
                  else
                    None)
            with
            | l           -> Some l
            | exception _ -> None)
        with
        | Some l -> Inl l
        | None ->
          Inr { fail = fun () ->
            die "\
No solution found for the following form in %s:
  (select %s from
    %s)"
              (Path.to_string dir)
              result_fn
              (String.concat ~sep:"\n    "
                 (List.map choices ~f:(fun c ->
                    Sexp.to_string (Lib_dep.sexp_of_choice c))))
          })
  in
  (List.concat libs, failures)

(* Pre-condition: all the dependencies of the library are already in
   the database *)
let create_lib_of_step1 t step1_db (dir, lib) =
  let requires, fails1 = interpret_lib_deps t ~dir lib.buildable.libraries in
  let ppx_runtime_deps, fails2 =
    inerpret_lib_deps t ~dir lib.ppx_runtime_libraries
  in
  let fail =
    match fail1 @ fail2 with
    | [] -> None
    | fail :: _ -> Some fail
  let requires =
    Lib.remove_dups_preserve_order
      (List.concat_map requires ~f:Lib.requires @ requires)
  in
  let ppx_runtime_deps =
    Lib.remove_dups_preserve_order
      (List.concat
         [ List.concat_map ppx_runtime_deps ~f:Lib.requires
         ; ppx_runtime_deps
         ; List.concat_map requires ~f:Lib.ppx_runtime_deps
         ])
  in
  let lib =
    { Lib.
        { name             : string
  ; dir              : Path.t
  ; optional         : bool
  ; public_c_headers : string list
  ; public           : Jbuild_types.Public_lib.t
  ; best_name        : string (** Either [name] or [p.name] if public = [Some p] *)
  ; has_stubs        : bool
  ; requires         : t list
  ; ppx_runtime_deps : t list
  ; (** If resolution of dependencies failed, this is the failure *)
    fail             : fail option
  ; resolved_selects : resolved_select list

  let pkg =
    { pkg.package with
      requires
    ; ppx_runtime_deps
    }
  in
  Hashtbl.add t.packages ~key:pkg.name ~data:(Present pkg)


let compute_instalable_internal_libs t ~internal_libraries =
  List.fold_left (top_sort_internals t ~internal_libraries) ~init:t
    ~f:(fun t (dir, lib) ->
      if not lib.Library.optional ||
         List.for_all (Library.all_lib_deps lib) ~f:(dep_is_installable t ~from:dir) then
        { t with
          instalable_internal_libs =
            String_map.add t.instalable_internal_libs
              ~key:lib.name ~data:(dir, lib)
        }
      else
        t)

let create findlib ~dirs_with_dot_opam_files internal_libraries =
  let step1 = Step1.create_db ~dirs_with_dot_opam_files internal_libraries in
  let sorted = top_sort_internals step1 ~internal_libraries in
  let t =
    { findlib
    ; by_public_name   = Hashtbl.create 1024
    ; by_internal_name = Hashtbl.create 1024
    ; instalable_internal_libs = String_map.empty
    }
  in

  (* Initializes the scopes *)
  Path.Set.iter dirs_with_dot_opam_files ~f:(fun dir ->
    Hashtbl.add t.by_internal_name ~key:dir
      ~data:(ref String_map.empty));
  List.iter internal_libraries ~f:(fun ((dir, lib) as internal) ->
    let scope = internal_name_scope t ~dir in
    scope := String_map.add !scope ~key:lib.Library.name ~data:internal;
    Option.iter lib.public ~f:(fun { name; _ } ->
      Hashtbl.add t.by_public_name ~key:name ~data:(Internal internal)));
  compute_instalable_internal_libs t ~internal_libraries

let internal_libs_without_non_installable_optional_ones t =
  String_map.values t.instalable_internal_libs

let interpret_lib_deps t ~dir lib_deps =
  let libs, failures =
    List.partition_map lib_deps ~f:(function
      | Lib_dep.Direct name -> begin
          match find t ~from:dir name with
          | x -> Inl [x]
          | exception e ->
            (* Call [find] again to get a proper backtrace *)
            Inr { fail = fun () -> ignore (find t ~from:dir name : Lib.t); raise e }
        end
      | Select { result_fn; choices } ->
        match
          List.find_map choices ~f:(fun { lits; _ } ->
            match
              List.filter_map lits ~f:(function
                | Pos s -> Some (find t ~from:dir s)
                | Neg s ->
                  if lib_is_installable t ~from:dir s then
                    raise Exit
                  else
                    None)
            with
            | l           -> Some l
            | exception _ -> None)
        with
        | Some l -> Inl l
        | None ->
          Inr { fail = fun () ->
            die "\
No solution found for the following form in %s:
  (select %s from
    %s)"
              (Path.to_string dir)
              result_fn
              (String.concat ~sep:"\n    "
                 (List.map choices ~f:(fun c ->
                    Sexp.to_string (Lib_dep.sexp_of_choice c))))
          })
  in
  let internals, externals =
    List.partition_map (List.concat libs) ~f:(function
      | Internal x -> Inl x
      | External x -> Inr x)
  in
  (internals, externals,
   match failures with
   | [] -> None
   | f :: _ -> Some f)

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

let resolve_selects t ~from lib_deps =
  List.filter_map lib_deps ~f:(function
    | Lib_dep.Direct _ -> None
    | Select { result_fn; choices } ->
      let src_fn =
        match List.find choices ~f:(choice_is_possible t ~from) with
        | Some c -> c.file
        | None -> "no solution found"
      in
      Some { dst_fn = result_fn; src_fn })
