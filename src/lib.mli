open Import

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

type t =
  | Internal of internal
  | External of Findlib.package

and internal =
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
  }

module Set : Set.S with type elt := t

val requires : t -> t list
val ppx_runtime_deps : t -> t list

val header_files : t list -> Path.t list

val include_paths : t list -> Path.Set.t

val include_flags : t list -> _ Arg_spec.t

val c_include_flags : t list -> _ Arg_spec.t

val link_flags : t list -> mode:Mode.t -> _ Arg_spec.t

val archive_files : t list -> mode:Mode.t -> ext_lib:string -> Path.t list

(** [public_name] if present, [name] if not *)
val best_name : t -> string

val describe : t -> string

val remove_dups_preserve_order : t list -> t list
