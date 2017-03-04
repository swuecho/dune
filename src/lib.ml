open Import

type resolved_select =
  { src_fn : string
  ; dst_fn : string

module T = struct
  type t =
    | Internal of internal
    | External of Findlib.package

  and internal =
    { name             : string
    ; dir              : Path.t
    ; optional         : bool
    ; public_c_headers : string list
    ; public           : Jbuild_types.Public_lib.t
    ; best_name        : string
    ; has_stubs        : bool
    ; requires         : t list
    ; ppx_runtime_deps : t list
    ; fail             : fail option
    ; resolved_selects : resolved_select list
    }

  let best_name = function
    | External pkg -> pkg.name
    | Internal lib -> lib.best_name

  let compare a b = String.compare (best_name a) (best_name b)
end

include T
module Set = Set.Make(T)

let dir = function
  | Internal lib -> lib.dir
  | External pkg -> pkg.dir

let requires = function
  | Internal lib -> lib.requires
  | External pkg -> List.map pkg.requires ~f:(fun pkg -> External pkg)

let ppx_runtime_deps = function
  | Internal lib -> lib.ppx_runtime_deps
  | External pkg -> List.map pkg.ppx_runtime_deps ~f:(fun pkg -> External pkg)

let header_files ts =
  List.fold_left ts ~init:[] ~f:(fun acc t ->
      match t with
      | External _ -> []
      | Internal lib ->
        match lib.install_c_headers with
        | [] -> acc
        | l ->
          List.fold_left l ~init:acc ~f:(fun acc fn ->
              Path.relative lib.dir (fn ^ ".h") :: acc))

let include_paths ts =
  List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
    Path.Set.add (dir t) acc)

let include_flags ts =
  let dirs = include_paths ts in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let has_headers = function
  | Internal lib -> lib.public_c_headers <> []
  | External pkg -> pkg.has_headers

let c_include_flags ts =
  let dirs =
    List.fold_left ts ~init:Path.Set.empty ~f:(fun acc t ->
      if has_headers t then
        Path.Set.add (dir t) acc
      else
        acc)
  in
  Arg_spec.S (List.concat_map (Path.Set.elements dirs) ~f:(fun dir ->
    [Arg_spec.A "-I"; Path dir]))

let describe = function
  | Internal lib ->
    sprintf "%s (local)" lib.best_name
  | External pkg ->
    sprintf "%s (external)" pkg.name

let link_flags ts ~mode =
  Arg_spec.S
    (include_flags ts ::
     List.map ts ~f:(fun t ->
       match t with
       | External pkg ->
         Arg_spec.Deps_rel (pkg.dir, Mode.Dict.get pkg.archives mode)
       | Internal lib ->
         Dep_rel (lib.dir, lib.name ^ Mode.compiled_lib_ext mode)))

let archive_files ts ~mode ~ext_lib =
  List.concat_map ts ~f:(function
    | External pkg ->
      List.map (Mode.Dict.get pkg.archives mode) ~f:(Path.relative pkg.dir)
    | Internal lib ->
      let l =
        [Path.relative lib.dir (lib.name ^ Mode.compiled_lib_ext mode)]
      in
      if lib.has_stubs then
        Utils.stubs_archive ~dir:lib.dir ~libname:lib.name ~ext_lib :: l
      else
        l)
(*
let ppx_runtime_libraries ts =
  List.fold_left ts ~init:String_set.empty ~f:(fun acc t ->
    match t with
    | Internal (_, lib) ->
      String_set.union acc (String_set.of_list lib.ppx_runtime_libraries)
    | External pkg ->
      String_set.union acc (String_set.of_list pkg.ppx_runtime_deps))
*)

let remove_dups_preserve_order libs =
  let rec loop seen libs acc =
    match libs with
    | [] -> List.rev acc
    | lib :: libs ->
      let name = best_name lib in
      if String_set.mem name seen then
        loop seen libs acc
      else
        loop (String_set.add name seen) libs (lib :: acc)
  in
  loop String_set.empty libs []
;;
