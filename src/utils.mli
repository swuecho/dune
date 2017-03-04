(** Utilities that can't go in [Import] *)

open Import

(** Return the absolute path to the shell, the argument to pass it (-c or /c) and a
    failure in case the shell can't be found. *)
val system_shell : needed_to:string -> Path.t * string * fail option

(** Return the path for the stubs archive of a library (libfoo_stubs.a for instance) *)
val stubs_archive : dir:Path.t -> libname:string -> ext_lib:string -> Path.t
