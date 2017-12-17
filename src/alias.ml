open! Import

(** Fully qualified name *)
module Fq_name : sig
  type t
  val pp : Format.formatter -> t -> unit
  val make : Path.t -> t
  val path : t -> Path.t
end = struct
  type t = Path.t
  let make t = t
  let path t = t
  let pp = Path.pp
end

type t =
  { name : Fq_name.t
  ; file : Path.t
  }
type alias = t

let pp fmt t =
  Format.fprintf fmt "@[<2>{ name@ =@ %a@ ;@ file@ =@ %a }@]"
    Path.pp (Fq_name.path t.name) Path.pp t.file

let aliases_path = Path.(relative root) "_build/.aliases"

let suffix = "-" ^ String.make 32 '0'

let of_path path =
  if not (Path.is_local path) then
    die "Aliases are only supported for local paths!\n\
         Tried to reference alias %S"
      (Path.to_string path);
  { name = Fq_name.make path
  ; file = Path.extend_basename (Path.append aliases_path path) ~suffix
  }

let name t = Path.basename (Fq_name.path t.name)
let dir  t = Path.parent   (Fq_name.path t.name)

let fully_qualified_name t = Fq_name.path t.name

let make name ~dir =
  assert (not (String.contains name '/'));
  of_path (Path.relative dir name)

let dep t = Build.path t.file

let is_standard = function
  | "runtest" | "install" | "doc" | "lint" -> true
  | _ -> false

let dep_rec ~loc ~file_tree t =
  let path = Path.parent   (Fq_name.path t.name) |> Path.drop_optional_build_context in
  let name = Path.basename (Fq_name.path t.name) in
  match File_tree.find_dir file_tree path with
  | None -> Build.fail { fail = fun () ->
    Loc.fail loc "Don't know about directory %s!" (Path.to_string_maybe_quoted path) }
  | Some dir ->
    let open Build.O in
    File_tree.Dir.fold dir ~traverse_ignored_dirs:false ~init:(Build.return true)
      ~f:(fun dir acc ->
        let path = File_tree.Dir.path dir in
        let t = of_path (Path.relative path name) in
        acc
        >>>
        Build.if_file_exists t.file
          ~then_:(Build.path t.file
                  >>^
                  fun _ -> false)
          ~else_:(Build.arr (fun x -> x)))
    >>^ fun is_empty ->
    if is_empty && not (is_standard name) then
      Loc.fail loc "This alias is empty.\n\
                    Alias %S is not defined in %s or any of its descendants."
        name (Path.to_string_maybe_quoted path)

let file t = t.file

let file_with_digest_suffix t ~digest =
  let dir = Path.parent t.file in
  let base = Path.basename t.file in
  let len = String.length base in
  Path.relative dir
    (String.sub base ~pos:0 ~len:(len - 32) ^ Digest.to_hex digest)

let of_file fn =
  match Path.extract_build_context fn with
  | Some (".aliases", fn) -> begin
      let dir  = Path.parent   fn in
      let name = Path.basename fn in
      match String.rsplit2 name ~on:'-' with
      | None -> assert false
      | Some (name, digest) ->
        assert (String.length digest = 32);
        Some (make name ~dir)
    end
  | _ -> None

let name_of_file fn =
  match Path.extract_build_context fn with
  | Some (".aliases", fn) -> begin
      let name = Path.basename fn in
      match String.rsplit2 name ~on:'-' with
      | None -> assert false
      | Some (name, digest) ->
        assert (String.length digest = 32);
        Some name
    end
  | _ -> None

let default = make "DEFAULT"
let runtest = make "runtest"
let install = make "install"
let doc     = make "doc"
let lint    = make "lint"

module Store = struct
  type entry =
    { alias : t
    ; mutable deps : Path.Set.t
    }
  let pp_entry fmt entry =
    let pp_deps fmt deps =
      Format.pp_print_list Path.pp fmt (Path.Set.elements deps) in
    Format.fprintf fmt "@[<2>{@ alias@ =@ %a@ ;@ deps@ = (%a)@ }@]"
      pp entry.alias pp_deps entry.deps

  type dir_status =
    { mutable dir_aliases : entry list
    ; (* Once the rules for a directory have been loaded, i.e.  [Build_system.load_dir
         build_system ~dir] has returned, this flag is set to [true]. After that, not more
         aliases in this directory can be defined or extended. *)
      mutable closed : bool
    }

  type files_of_dir =
    { files_by_ext : Path.t list String_map.t
    ; dir_hash     : string
    ; mutable aliases : alias String_map.t
    }

  type t =
    { aliases : (Fq_name.t, entry) Hashtbl.t
    ; per_dir : (Path.t, dir_status) Hashtbl.t
    ; files_of : (Path.t, files_of_dir) Hashtbl.t
    ; build_system : Build_system.t
    }

  let pp fmt (t : t) =
    let bindings = Hashtbl.fold t.aliases ~init:[] ~f:(fun ~key ~data acc ->
      (key, data)::acc
    ) in
    let pp_bindings fmt b =
      Format.pp_print_list (fun fmt (k, v) ->
        Format.fprintf fmt "@[<2>(%a@ %a)@]" Fq_name.pp k pp_entry v
      ) fmt b in
    Format.fprintf fmt "Store.t@ @[@<2>(%a)@]" pp_bindings bindings

  let create build_system =
    { aliases  = Hashtbl.create 1024
    ; per_dir  = Hashtbl.create 1024
    ; files_of = Hashtbl.create 1024
    ; build_system
    }

  let unlink (store : t) = function
    | [] -> ()
    | alias_basenames ->
      store.aliases
      |> Hashtbl.fold ~init:Path.Set.empty ~f:(fun ~key:_ ~data:entry acc ->
        if List.mem (name entry.alias) ~set:alias_basenames then (
          Path.Set.union acc (Path.Set.add entry.alias.file entry.deps)
        ) else (
          acc
        ))
      |> Path.Set.iter ~f:Path.unlink_no_err
end

let add_deps (store : Store.t) t deps =
  let deps = Path.Set.of_list deps in
  let dir_status =
    Hashtbl.find_or_add store.per_dir (Path.parent (Fq_name.path t.name))
      ~f:(fun _ -> { Store. dir_aliases = []; closed = false })
  in
  assert (not dir_status.closed);
  match Hashtbl.find store.aliases t.name with
  | None ->
    let entry =
      { Store.alias = t
      ; deps = deps
      }
    in
    Hashtbl.add store.aliases ~key:t.name ~data:entry;
    dir_status.dir_aliases <- entry :: dir_status.dir_aliases;
  | Some e ->
    e.deps <- Path.Set.union deps e.deps

let add_build store t ~stamp build =
  let digest = Digest.string (Sexp.to_string stamp) in
  let digest_path = file_with_digest_suffix t ~digest in
  add_deps store t [digest_path];
  Build.progn
    [ build
    ; Build.create_file digest_path
    ]

let add_builds store t builds =
  let digest_files, actions =
    List.split
      (List.map builds ~f:(fun (stamp, build) ->
         let digest = Digest.string (Sexp.to_string stamp) in
         let digest_path = file_with_digest_suffix t ~digest in
         (digest_path,
          Build.progn
            [ build
            ; Build.create_file digest_path
            ])))
  in
  add_deps store t digest_files;
  actions

let files_of (store : Store.t) ~dir ~ext =
  let files_of_dir =
    Hashtbl.find_or_add store.files_of dir ~f:(fun dir ->
      let files_by_ext =
        Build_system.targets_of store.build_system ~dir
        |> Path.Set.elements
        |> List.map ~f:(fun fn -> Filename.extension (Path.to_string fn), fn)
        |> String_map.of_alist_multi
      in
      { files_by_ext
      ; dir_hash = Path.to_string dir |> Digest.string |> Digest.to_hex
      ; aliases = String_map.empty
      })
  in
  match String_map.find ext files_of_dir.aliases with
  | Some alias -> alias
  | None ->
    let alias =
      make ~dir:(Path.of_string (sprintf ".external-files/%s%s" files_of_dir.dir_hash ext))
        "files"
    in
    add_deps store alias
      (Option.value
         (String_map.find ext files_of_dir.files_by_ext)
         ~default:[]);
    files_of_dir.aliases <- String_map.add files_of_dir.aliases ~key:ext ~data:alias;
    alias

let gen_rules (store : Store.t) ~context_names dir =
  if Path.is_local dir then begin
    let build_system = store.build_system in
    Build_system.load_dir build_system ~dir;
    if not (Path.is_in_build_dir dir) then begin
      let aliases =
        List.concat_map context_names ~f:(fun name ->
          let dir = Path.append (Path.of_string ("_build/" ^ name)) dir in
          Build_system.load_dir build_system ~dir;
          Build_system.load_dir build_system ~dir:(Path.append aliases_path dir);
          match Hashtbl.find store.Store.per_dir dir with
          | None -> []
          | Some { dir_aliases; closed } ->
            assert closed;
            dir_aliases)
      in
      List.iter aliases ~f:(fun { Store. alias; _ } ->
        add_deps store (of_path (Path.drop_build_context (Fq_name.path alias.name)))
          [alias.file])
    end;
    match Hashtbl.find store.per_dir dir with
    | None -> ()
    | Some d ->
      d.closed <- true;
      List.iter d.dir_aliases ~f:(fun { Store. alias; deps } ->
        let open Build.O in
        Build_system.add_rule build_system
          (Build_interpret.Rule.make
             (Build.path_set deps >>>
              Build.action ~targets:[alias.file]
                (Redirect (Stdout,
                           alias.file,
                           Digest_files
                             (Path.Set.elements deps))))))
  end
