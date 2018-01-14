(** Build rules *)

open! Import

type t

(** Create a new build system. [file_tree] represent the source
    tree. [gen_rules] is used to generate the rules for a given
    directory.

    It is expected that [gen_rules] only generate rules whose targets
    are descendant of [dir]. *)
val create
  :  contexts:Context.t list
  -> file_tree:File_tree.t
  -> gen_rules:(t -> dir:Path.t -> unit)
  -> t

(** Changes the rule generator callback *)
val set_rule_generator : t -> f:(t -> dir:Path.t -> unit) -> unit

(** Add a rule to the system. This function must be called from the [gen_rules]
    callback. All the target of the rule must be in the same directory.

    Assuming that [gen_rules ~dir:a] calls [add_rule r] where [r.dir] is [Some b], one of
    the following assumption must hold:

    - [a] and [b] are the same
    - [gen_rules ~dir:b] calls [load_dir ~dir:a]

    The call to [load_dir ~dir:a] from [gen_rules ~dir:b] declares a directory dependency
    from [b] to [a]. There must be no cyclic directory dependencies.
*)
val add_rule : t -> Build_interpret.Rule.t -> unit

(** [eval_glob t ~dir re ~f] returns the list of files in [dir] that matches [re] to
    [f]. The list of files includes the list of targets. *)
val eval_glob : t -> dir:Path.t -> Re.re -> string list

(** Returns the set of targets in the given directory. *)
val targets_of : t -> dir:Path.t -> Path.Set.t

(** Load the rules for this directory. *)
val load_dir : t -> dir:Path.t -> unit

(** [on_load_dir ~dir ~f] remembers to run [f] when loading the rules for [dir]. *)
val on_load_dir : t -> dir:Path.t -> f:(unit -> unit) -> unit

val is_target : t -> Path.t -> bool

module Build_error : sig
  type t

  val backtrace : t -> Printexc.raw_backtrace
  val dependency_path : t -> Path.t list
  val exn : t -> exn

  exception E of t
end

(** Do the actual build *)
val do_build
  :  t
  -> request:(unit, unit) Build.t
  -> (unit Future.t, Build_error.t) result
val do_build_exn
  :  t
  -> request:(unit, unit) Build.t
  -> unit Future.t

(** Return all the library dependencies (as written by the user)
   needed to build this request *)
val all_lib_deps
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps Path.Map.t

(** Return all the library dependencies required to build this
   request, by context name *)
val all_lib_deps_by_context
  :  t
  -> request:(unit, unit) Build.t
  -> Build.lib_deps String_map.t

(** List of all buildable targets *)
val all_targets : t -> Path.t list

(** A fully built rule *)
module Rule : sig
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> int
  end

  type t =
    { id      : Id.t
    ; deps    : Path.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }
end

(** Return the list of rules used to build the given targets. If
    [recursive] is [true], return all the rules needed to build the
    given targets and their transitive dependencies. *)
val build_rules
  :  ?recursive:bool (* default false *)
  -> t
  -> request:(unit, unit) Build.t
  -> Rule.t list Future.t

val all_targets_ever_built
  :  unit
  -> Path.t list

val dump_trace : t -> unit
