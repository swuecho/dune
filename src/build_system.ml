open Import
open Future

module Pset  = Path.Set
module Pmap  = Path.Map
module Vspec = Build.Vspec

module Exec_status = struct
  module Starting = struct
    type t = { for_file : Path.t }
  end
  module Evaluating_rule = struct
    type t =
      { for_file        : Path.t
      ; rule_evaluation : (Action.t * Pset.t) Future.t
      ; exec_rule       : targeting:Path.t
          -> (Action.t * Pset.t) Future.t -> unit Future.t
      }
  end
  module Running = struct
    type t =
      { for_file        : Path.t
      ; (* Future that only waits for the evaluation of the rule to terminate. It holds
           the computed action and dynamic dependencies. *)
        rule_evaluation : (Action.t * Pset.t) Future.t
      ; (* Future that waits for the rule's action to terminate *)
        rule_execution  : unit Future.t
      }
  end
  module Not_started = struct
    type t =
      { eval_rule : targeting:Path.t -> (Action.t * Pset.t) Future.t
      ; exec_rule : targeting:Path.t -> (Action.t * Pset.t) Future.t -> unit Future.t
      }
  end
  type t =
    | Not_started     of Not_started.t
    | Starting        of Starting.t
    | Evaluating_rule of Evaluating_rule.t
    | Running         of Running.t
end

module Internal_rule = struct
  module Id : sig
    type t
    val to_int : t -> int
    val compare : t -> t -> int
    val gen : unit -> t
  end = struct
    type t = int
    let to_int x = x
    let compare (x : int) y = compare x y

    let counter = ref 0
    let gen () =
      let n = !counter in
      counter := n + 1;
      n
  end

  module Fallback_status = struct
    type t =
      (* The argument is the set of targets that are already present in the source
         tree.  *)
      | Yes of Pset.t
      | No
      | Not_possible
  end

  type t =
    { id               : Id.t
    ; rule_deps        : Pset.t
    ; static_deps      : Pset.t
    ; targets          : Pset.t
    ; context          : Context.t option
    ; build            : (unit, Action.t) Build.t
    ; mutable fallback : Fallback_status.t
    ; loc              : Loc.t option
    ; mutable exec     : Exec_status.t
    }

  let compare a b = Id.compare a.id b.id

  let loc ~dir t =
    match t.loc with
    | Some loc -> loc
    | None ->
      Loc.in_file
        (Path.to_string
           (Path.drop_optional_build_context (Path.relative dir "jbuild")))
end

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file       : 'a Vfile_kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Some Eq
    | Sexp_file a    , Sexp_file b     -> Vfile_kind.eq a b
    | _                                -> None

  let eq_exn a b = Option.value_exn (eq a b)
end

module File_spec = struct
  type 'a t =
    { rule         : Internal_rule.t (* Rule which produces it *)
    ; mutable kind : 'a File_kind.t
    ; mutable data : 'a option
    }

  type packed = T : _ t -> packed

  let create rule kind =
    T { rule; kind; data = None }
end

module Dir_status = struct
  type waiting_for_load_dir =
    { mutable lazy_generators : (unit -> unit) list }

  type collection_stage =
    | Loading
    | Pending of waiting_for_load_dir

  type rules_collector =
    { mutable rules : Build_interpret.Rule.t list
    ; mutable stage : collection_stage
    }

  type t =
    | Collecting_rules of rules_collector
    | Loaded of Pset.t (* set of targets in the directory *)
end

type t =
  { (* File specification by targets *)
    files       : (Path.t, File_spec.packed) Hashtbl.t
  ; contexts    : Context.t list
  ; (* Table from target to digest of
       [(deps (filename + contents), targets (filename only), action)] *)
    trace       : (Path.t, Digest.t) Hashtbl.t
  ; file_tree   : File_tree.t
  ; mutable local_mkdirs : Path.Local.Set.t
  ; mutable dirs : (Path.t, Dir_status.t) Hashtbl.t
  ; mutable gen_rules : (t -> dir:Path.t -> unit)
  ; mutable load_dir_stack : Path.t list
  ; (* Set of directories under _build that have at least one rule and
       all their ancestors. *)
    mutable build_dirs_to_keep : Path.Set.t
  }

let set_rule_generator t ~f = t.gen_rules <- f

let get_dir_status t ~dir =
  Hashtbl.find_or_add t.dirs dir ~f:(fun _ ->
    Dir_status.Collecting_rules
      { rules = []
      ; stage = Pending { lazy_generators = [] }
      })

let find_file_exn t file =
  Hashtbl.find_exn t.files file
    ~string_of_key:(fun fn -> sprintf "%S" (Path.to_string fn))
    ~table_desc:(fun _ -> "<target to rule>")

module Build_error = struct
  type t =
    { backtrace : Printexc.raw_backtrace
    ; dep_path  : Path.t list
    ; exn       : exn
    }

  let backtrace t = t.backtrace
  let dependency_path t = t.dep_path
  let exn t = t.exn

  exception E of t

  let raise t ~targeting ~backtrace exn =
    let rec build_path acc targeting ~seen =
      assert (not (Pset.mem targeting seen));
      let seen = Pset.add targeting seen in
      let (File_spec.T file) = find_file_exn t targeting in
      match file.rule.exec with
      | Not_started _ -> assert false
      | Running { for_file; _ } | Starting { for_file }
      | Evaluating_rule { for_file; _ } ->
        if for_file = targeting then
          acc
        else
          build_path (for_file :: acc) for_file ~seen
    in
    let dep_path = build_path [targeting] targeting ~seen:Pset.empty in
    raise (E { backtrace; dep_path; exn })
end

let wrap_build_errors t ~f ~targeting =
  with_exn_handler (fun () -> f ~targeting)
    ~handler:(fun exn backtrace ->
      match exn with
      | Build_error.E _ -> reraise exn
      | exn -> Build_error.raise t exn ~targeting ~backtrace)

module Target = Build_interpret.Target
module Pre_rule = Build_interpret.Rule

let get_file : type a. t -> Path.t -> a File_kind.t -> a File_spec.t = fun t fn kind ->
  match Hashtbl.find t.files fn with
  | None -> die "no rule found for %s" (Path.to_string fn)
  | Some (File_spec.T file) ->
    let Eq = File_kind.eq_exn kind file.kind in
    file

let vfile_to_string (type a) (module K : Vfile_kind.S with type t = a) fn x =
  K.to_string fn x

module Build_exec = struct
  open Build.Repr

  let exec bs t x =
    let rec exec
      : type a b. Pset.t ref -> (a, b) t -> a -> b = fun dyn_deps t x ->
      match t with
      | Arr f -> f x
      | Targets _ -> x
      | Store_vfile (Vspec.T (fn, kind)) ->
        let file = get_file bs fn (Sexp_file kind) in
        file.data <- Some x;
        Write_file (fn, vfile_to_string kind fn x)
      | Compose (a, b) ->
        exec dyn_deps a x |> exec dyn_deps b
      | First t ->
        let x, y = x in
        (exec dyn_deps t x, y)
      | Second t ->
        let x, y = x in
        (x, exec dyn_deps t y)
      | Split (a, b) ->
        let x, y = x in
        let x = exec dyn_deps a x in
        let y = exec dyn_deps b y in
        (x, y)
      | Fanout (a, b) ->
        let a = exec dyn_deps a x in
        let b = exec dyn_deps b x in
        (a, b)
      | Paths _ -> x
      | Paths_glob state -> get_glob_result_exn state
      | Contents p -> Io.read_file (Path.to_string p)
      | Lines_of p -> Io.lines_of_file (Path.to_string p)
      | Vpath (Vspec.T (fn, kind)) ->
        let file : b File_spec.t = get_file bs fn (Sexp_file kind) in
        Option.value_exn file.data
      | Dyn_paths t ->
        let fns = exec dyn_deps t x in
        dyn_deps := Pset.union !dyn_deps (Pset.of_list fns);
        x
      | Record_lib_deps _ -> x
      | Fail { fail } -> fail ()
      | If_file_exists (_, state) ->
        exec dyn_deps (get_if_file_exists_exn state) x
      | Memo m ->
        match m.state with
        | Evaluated (x, deps) ->
          dyn_deps := Pset.union !dyn_deps deps;
          x
        | Evaluating ->
          die "Dependency cycle evaluating memoized build arrow %s" m.name
        | Unevaluated ->
          m.state <- Evaluating;
          let dyn_deps' = ref Pset.empty in
          let x = exec dyn_deps' m.t x in
          m.state <- Evaluated (x, !dyn_deps');
          dyn_deps := Pset.union !dyn_deps !dyn_deps';
          x
    in
    let dyn_deps = ref Pset.empty in
    let action = exec dyn_deps (Build.repr t) x in
    (action, !dyn_deps)

  let exec_nop bs t x =
    snd (exec bs (Build.O.(>>^) t (fun () -> Action.Progn [])) x)
end

(* This variable is filled during the creation of the build system. Once the build system
   is created, we check that all the fallback rules that got disabled are completely
   disabled, i.e. that all their targets already exist in the source tree. *)
let disabled_fallback_rules = ref []

(* [copy_source] is [true] for rules copying files from the source directory *)
let add_spec t fn spec ~copy_source =
  match Hashtbl.find t.files fn with
  | None ->
    Hashtbl.add t.files ~key:fn ~data:spec
  | Some (File_spec.T { rule; _ }) ->
    match copy_source, rule.fallback with
    | true, Yes already_present ->
      if Pset.is_empty already_present then
        disabled_fallback_rules := rule :: !disabled_fallback_rules;
      rule.fallback <- Yes (Pset.add fn already_present);
      Hashtbl.add t.files ~key:fn ~data:spec
    | true, (No | Not_possible) ->
      Loc.warn (Internal_rule.loc rule ~dir:(Path.parent fn))
        "File %s is both generated by a rule and present in the source tree.\n\
         As a result, the rule is currently ignored, however this will become an error \
         in the future.\n\
         %t"
        (maybe_quoted (Path.basename fn))
        (fun ppf ->
           match rule.fallback with
           | Yes _ -> assert false
           | Not_possible ->
             Format.fprintf ppf "Delete file %s to get rid of this warning."
               (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
           | No ->
             Format.fprintf ppf
               "To keep the current behavior and get rid of this warning, add a field \
                (fallback) to the rule.");
      Hashtbl.add t.files ~key:fn ~data:spec
    | false, _ ->
      die "multiple rules generated for %s" (Path.to_string_maybe_quoted fn)

let create_file_specs t targets rule ~copy_source =
  List.iter targets ~f:(function
    | Target.Normal fn ->
      add_spec t fn (File_spec.create rule Ignore_contents) ~copy_source
    | Target.Vfile (Vspec.T (fn, kind)) ->
      add_spec t fn (File_spec.create rule (Sexp_file kind)) ~copy_source)

(* This contains the targets of the actions that are being executed. On exit, we need to
   delete them as they might contain garbage *)
let pending_targets = ref Pset.empty

let () =
  Future.Scheduler.at_exit_after_waiting_for_commands (fun () ->
    let fns = !pending_targets in
    pending_targets := Pset.empty;
    Pset.iter fns ~f:Path.unlink_no_err)

let clear_targets_digests_after_rule_execution targets =
  let missing =
    List.fold_left targets ~init:Pset.empty ~f:(fun acc fn ->
      match Unix.lstat (Path.to_string fn) with
      | exception _ -> Pset.add fn acc
      | (_ : Unix.stats) ->
        Utils.Cached_digest.remove fn;
        acc)
  in
  if not (Pset.is_empty missing) then
    die "@{<error>Error@}: Rule failed to generate the following targets:\n%s"
      (Pset.elements missing
       |> List.map ~f:(fun fn -> sprintf "- %s" (Path.to_string fn))
       |> String.concat ~sep:"\n")

let make_local_dirs t paths =
  Pset.iter paths ~f:(fun path ->
    match Path.kind path with
    | Local path ->
      if not (Path.Local.Set.mem path t.local_mkdirs) then begin
        Path.Local.mkdir_p path;
        t.local_mkdirs <- Path.Local.Set.add path t.local_mkdirs
      end
    | _ -> ())

let make_local_parent_dirs t paths ~map_path =
  Pset.iter paths ~f:(fun path ->
    match Path.kind (map_path path) with
    | Local path when not (Path.Local.is_root path) ->
      let parent = Path.Local.parent path in
      if not (Path.Local.Set.mem parent t.local_mkdirs) then begin
        Path.Local.mkdir_p parent;
        t.local_mkdirs <- Path.Local.Set.add parent t.local_mkdirs
      end
    | _ -> ())

let sandbox_dir = Path.of_string "_build/.sandbox"

let locks : (Path.t, Future.Mutex.t) Hashtbl.t = Hashtbl.create 32

let rec with_locks mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Future.Mutex.with_lock
      (Hashtbl.find_or_add locks m ~f:(fun _ -> Future.Mutex.create ()))
      (fun () -> with_locks mutexes ~f)

let remove_old_artifacts t ~dir =
  if not (Path.is_in_build_dir dir) || Hashtbl.mem t.files (Path.relative dir Config.jbuilder_keep_fname) then
    ()
  else
    match Path.readdir dir with
    | exception _ -> ()
    | files ->
      List.iter files ~f:(fun fn ->
        let fn = Path.relative dir fn in
        match Unix.lstat (Path.to_string fn) with
        | { st_kind = S_DIR; _ } -> ()
        | exception _ ->
          if not (Hashtbl.mem t.files fn) then Path.unlink fn
        | _ ->
          if not (Hashtbl.mem t.files fn) then Path.unlink fn)

let rec compile_rule t ?(copy_source=false) pre_rule =
  let { Pre_rule.
        context
      ; build
      ; targets = target_specs
      ; sandbox
      ; fallback
      ; locks
      ; loc
      ; dir = _
      } =
    pre_rule
  in
  let targets = Target.paths target_specs in
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps = static_deps
      } = Build_interpret.static_deps build ~all_targets:(load_dir t)
  in

  let eval_rule ~targeting =
    wait_for_deps t rule_deps ~targeting
    >>| fun () ->
    Build_exec.exec t build ()
  in
  let exec_rule ~targeting rule_evaluation =
    make_local_parent_dirs t targets ~map_path:(fun x -> x);
    Future.both
      (wait_for_deps t static_deps ~targeting)
      (rule_evaluation >>= fun (action, dyn_deps) ->
       wait_for_deps t ~targeting (Pset.diff dyn_deps static_deps)
       >>| fun () ->
       (action, dyn_deps))
    >>= fun ((), (action, dyn_deps)) ->
    let all_deps = Pset.union static_deps dyn_deps in
    let all_deps_as_list = Pset.elements all_deps in
    let targets_as_list  = Pset.elements targets  in
    let hash =
      let trace =
        (List.map all_deps_as_list ~f:(fun fn ->
           (fn, Utils.Cached_digest.file fn)),
         targets_as_list,
         Option.map context ~f:(fun c -> c.name),
         action)
      in
      Digest.string (Marshal.to_string trace [])
    in
    let sandbox_dir =
      if sandbox then
        Some (Path.relative sandbox_dir (Digest.to_hex hash))
      else
        None
    in
    let deps_or_rule_changed =
      List.fold_left targets_as_list ~init:false ~f:(fun acc fn ->
        match Hashtbl.find t.trace fn with
        | None ->
          Hashtbl.add t.trace ~key:fn ~data:hash;
          true
        | Some prev_hash ->
          Hashtbl.replace t.trace ~key:fn ~data:hash;
          acc || prev_hash <> hash)
    in
    let targets_missing =
      List.exists targets_as_list ~f:(fun fn ->
        match Unix.lstat (Path.to_string fn) with
        | exception _ -> true
        | (_ : Unix.stats) -> false)
    in
    if deps_or_rule_changed || targets_missing then (
      (* Do not remove files that are just updated, otherwise this would break incremental
         compilation *)
      let targets_to_remove =
        Pset.diff targets (Action.updated_files action)
      in
      Pset.iter targets_to_remove ~f:Path.unlink_no_err;
      pending_targets := Pset.union targets_to_remove !pending_targets;
      let action =
        match sandbox_dir with
        | Some sandbox_dir ->
          Path.rm_rf sandbox_dir;
          let sandboxed path =
            if Path.is_local path then
              Path.append sandbox_dir path
            else
              path
          in
          make_local_parent_dirs t all_deps ~map_path:sandboxed;
          make_local_parent_dirs t targets  ~map_path:sandboxed;
          Action.sandbox action
            ~sandboxed
            ~deps:all_deps_as_list
            ~targets:targets_as_list
        | None ->
          action
      in
      make_local_dirs t (Action.chdirs action);
      with_locks locks ~f:(fun () ->
        Action.exec ?context ~targets action) >>| fun () ->
      Option.iter sandbox_dir ~f:Path.rm_rf;
      (* All went well, these targets are no longer pending *)
      pending_targets := Pset.diff !pending_targets targets_to_remove;
      clear_targets_digests_after_rule_execution targets_as_list
    ) else
      return ()
  in
  let rule =
    { Internal_rule.
      id = Internal_rule.Id.gen ()
    ; static_deps
    ; rule_deps
    ; targets
    ; build
    ; context
    ; exec = Not_started { eval_rule; exec_rule }
    ; fallback = (match fallback with
      | Yes -> Yes Pset.empty
      | No -> No
      | Not_possible -> Not_possible)
    ; loc
    }
  in
  create_file_specs t target_specs rule ~copy_source

and setup_copy_rules t ~ctx_dir ~non_target_source_files =
  Pset.iter non_target_source_files ~f:(fun path ->
    let ctx_path = Path.append ctx_dir path in
    if is_target t ctx_path &&
       String.is_suffix (Path.basename ctx_path) ~suffix:".install" then
      (* Do not copy over .install files that are generated by a rule. *)
      ()
    else
      let build = Build.copy ~src:path ~dst:ctx_path in
      (* We temporarily allow overrides while setting up copy rules
         from the source directory so that artifact that are already
         present in the source directory are not re-computed.

         This allows to keep generated files in tarballs. Maybe we
         should allow it on a case-by-case basis though. *)
      compile_rule t (Pre_rule.make build) ~copy_source:true)

and is_target t file =
  Pset.mem file (load_dir t ~dir:(Path.parent file))

and load_dir t ~dir =
  match get_dir_status t ~dir with
  | Loaded targets -> targets

  | Collecting_rules collector ->
    let lazy_generators =
      match collector.stage with
      | Loading ->
        die "recursive dependency between directories:\n  %s"
          (String.concat ~sep:"\n--> "
             (List.map t.load_dir_stack ~f:Utils.describe_target))
      | Pending { lazy_generators } ->
        collector.stage <- Loading;
        lazy_generators
    in

    collector.stage <- Loading;
    t.load_dir_stack <- dir :: t.load_dir_stack;
    List.iter lazy_generators ~f:(fun f -> f ());

    (* Load all the rules *)
    t.gen_rules t ~dir;
    let rules = collector.rules in

    (* Compute the set of targets and files to copy *)
    let user_rule_targets =
      List.fold_left rules ~init:Pset.empty ~f:(fun acc { Pre_rule.targets; _ } ->
        List.fold_left targets ~init:acc ~f:(fun acc target ->
          let path = Target.path target in
          Pset.add path acc))
    in
    let targets, to_copy, subdirs_deletion =
      match Path.extract_build_context_dir dir with
      | None ->
        let files =
          if Path.is_local dir then
            File_tree.files_of t.file_tree dir
          else
            match Path.readdir dir with
            | exception _ -> Path.Set.empty
            | files ->
              Pset.of_list (List.map files ~f:(Path.relative dir))
        in
        (Pset.union user_rule_targets files, None, Keep_all)
      | Some (ctx_path, path) ->
        match Path.basename ctx_path with
        | "install" ->
          (user_rule_targets, None, Delete_all_unkown)
        | ".aliases" ->
          (user_rule_targets, None,
           Keep_these
             (let path = Path.drop_build_context path in
              match File_tree.find_dir t.file_tree path with
              | None -> String_set.empty
              | 
          
          let files = File_tree.files_of t.file_tree path in
          if Pset.is_empty files then
            (user_rule_targets, None)
          else
            (Pset.union user_rule_targets
               (Pset.map files ~f:(Path.append ctx_path)),
             Some (ctx_path, files))
    in

    (* Set the directory status to loaded *)
    Hashtbl.replace t.dirs ~key:dir ~data:(Loaded targets);
    (match t.load_dir_stack with
     | [] -> assert false
     | x :: l ->
       t.load_dir_stack <- l;
       assert (x = dir));

    (* Compile the rules *)
    List.iter rules ~f:(compile_rule t ~copy_source:false);
    Option.iter to_copy ~f:(fun (ctx_dir, source_files) ->
      setup_copy_rules t ~ctx_dir ~non_target_source_files:source_files);
    remove_old_artifacts t ~dir;
    (let l = !disabled_fallback_rules in
     disabled_fallback_rules := [];
     List.iter l ~f:(fun rule ->
       let disabled_for =
         match rule.Internal_rule.fallback with
         | No | Not_possible -> assert false
         | Yes paths -> paths
       in
       let leftover_targets = Pset.diff rule.targets disabled_for in
       if not (Pset.is_empty leftover_targets) then begin
         let list_paths set =
           Pset.elements set
           |> List.map ~f:(fun p -> sprintf "- %s"
                                      (Path.to_string_maybe_quoted
                                         (Path.drop_optional_build_context p)))
           |> String.concat ~sep:"\n"
         in
         Loc.fail (Internal_rule.loc rule ~dir:(Path.parent (Pset.choose leftover_targets)))
           "\
Some of the targets of this fallback rule are present in the source tree,
and some are not. This is not allowed. Either none of the targets must
be present in the source tree, either they must all be.

The following targets are present:
%s

The following targets are not:
%s
"
           (list_paths disabled_for)
           (list_paths leftover_targets)
       end
     ));
    targets

and load_dir_unit t ~dir = ignore (load_dir t ~dir : Pset.t)

and wait_for_file t fn ~targeting =
  let dir = Path.parent fn in
  if Path.is_local dir then load_dir_unit t ~dir;
  match Hashtbl.find t.files fn with
  | None ->
    if Path.is_in_build_dir fn then
      die "no rule found for %s" (Utils.describe_target fn)
    else if Path.exists fn then
      return ()
    else
      die "file unavailable: %s" (Path.to_string fn)
  | Some (File_spec.T file) ->
    match file.rule.exec with
    | Not_started { eval_rule; exec_rule } ->
      file.rule.exec <- Starting { for_file = targeting };
      let rule_evaluation =
        wrap_build_errors t ~targeting:fn ~f:eval_rule
      in
      let rule_execution =
        wrap_build_errors t ~targeting:fn ~f:(exec_rule rule_evaluation)
      in
      file.rule.exec <-
        Running { for_file = targeting
                ; rule_evaluation
                ; rule_execution
                };
      rule_execution
    | Running { rule_execution; _ } -> rule_execution
    | Evaluating_rule { for_file; rule_evaluation; exec_rule } ->
      file.rule.exec <- Starting { for_file = targeting };
      let rule_execution =
        wrap_build_errors t ~targeting:fn ~f:(exec_rule rule_evaluation)
      in
      file.rule.exec <-
        Running { for_file
                ; rule_evaluation
                ; rule_execution
                };
      rule_execution
    | Starting _ ->
      (* Recursive deps! *)
      let rec build_loop acc targeting =
        let acc = targeting :: acc in
        if fn = targeting then
          acc
        else
          let (File_spec.T file) = find_file_exn t targeting in
          match file.rule.exec with
          | Not_started _ | Running _ | Evaluating_rule _ -> assert false
          | Starting { for_file } ->
            build_loop acc for_file
      in
      let loop = build_loop [fn] targeting in
      die "Dependency cycle between the following files:\n    %s"
        (String.concat ~sep:"\n--> "
           (List.map loop ~f:Path.to_string))

and wait_for_deps t deps ~targeting =
  all_unit
    (Pset.fold deps ~init:[] ~f:(fun fn acc -> wait_for_file t fn ~targeting :: acc))

module Trace = struct
  type t = (Path.t, Digest.t) Hashtbl.t

  let file = "_build/.db"

  let dump (trace : t) =
    Utils.Cached_digest.dump ();
    let sexp =
      Sexp.List (
        Hashtbl.fold trace ~init:Pmap.empty ~f:(fun ~key ~data acc ->
          Pmap.add acc ~key ~data)
        |> Path.Map.bindings
        |> List.map ~f:(fun (path, hash) ->
          Sexp.List [ Atom (Path.to_string path); Atom (Digest.to_hex hash) ]))
    in
    if Sys.file_exists "_build" then
      Io.write_file file (Sexp.to_string sexp)

  let load () =
    Utils.Cached_digest.load ();
    let trace = Hashtbl.create 1024 in
    if Sys.file_exists file then begin
      let sexp = Sexp.load ~fname:file ~mode:Single in
      let bindings =
        let open Sexp.Of_sexp in
        list (pair Path.t (fun s -> Digest.from_hex (string s))) sexp
      in
      List.iter bindings ~f:(fun (path, hash) ->
        Hashtbl.add trace ~key:path ~data:hash);
    end;
    trace
end

let all_targets_ever_built () =
  if Sys.file_exists Trace.file then
    let trace = Trace.load () in
    Hashtbl.fold trace ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)
  else
    []

let all_targets t =
  List.iter t.contexts ~f:(fun ctx ->
    File_tree.fold t.file_tree ~traverse_ignored_dirs:true ~init:() ~f:(fun dir () ->
      load_dir_unit t ~dir:(Path.append ctx.Context.build_dir (File_tree.Dir.path dir))));
  Hashtbl.fold t.files ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

let dump_trace t = Trace.dump t.trace

let create ~contexts ~file_tree ~gen_rules =
  let t =
    { contexts
    ; files      = Hashtbl.create 1024
    ; trace      = Trace.load ()
    ; local_mkdirs = Path.Local.Set.empty
    ; dirs       = Hashtbl.create 1024
    ; load_dir_stack = []
    ; file_tree
    ; gen_rules
    }
  in
  at_exit (fun () -> dump_trace t);
  Future.Scheduler.at_exit_after_waiting_for_commands Action.Promotion.finalize;
  t

let eval_request t ~request ~process_target =
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps = static_deps
      } = Build_interpret.static_deps request ~all_targets:(load_dir t)
  in

  let process_targets ts =
    Future.all_unit (List.map (Pset.elements ts) ~f:process_target)
  in

  Future.both
    (process_targets static_deps)
    (Future.all_unit (List.map (Pset.elements rule_deps) ~f:(fun fn ->
       wait_for_file t fn ~targeting:fn))
     >>= fun () ->
     let dyn_deps = Build_exec.exec_nop t request () in
     process_targets (Pset.diff dyn_deps static_deps))
  >>| fun ((), ()) -> ()

let do_build_exn t ~request =
  eval_request t ~request ~process_target:(fun fn ->
    wait_for_file t fn ~targeting:fn)

let do_build t ~request =
  try
    Ok (do_build_exn t ~request)
  with Build_error.E e ->
    Error e

module Ir_set = Set.Make(Internal_rule)

let rules_for_files t paths =
  List.filter_map paths ~f:(fun path ->
    match Hashtbl.find t.files path with
    | None -> None
    | Some (File_spec.T { rule; _ }) -> Some rule)
  |> Ir_set.of_list
  |> Ir_set.elements

module Ir_closure =
  Top_closure.Make(Internal_rule.Id)
    (struct
      type graph = t
      type t = Internal_rule.t
      let key (t : t) = t.id
      let deps (t : t) bs =
        rules_for_files bs
          (Pset.elements
             (Pset.union
                t.static_deps
                t.rule_deps))
    end)

let rules_for_targets t targets =
  match Ir_closure.top_closure t (rules_for_files t targets) with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun rule ->
         Path.to_string (Pset.choose rule.Internal_rule.targets))
       |> String.concat ~sep:"\n-> ")

let static_deps_of_request t request =
  let { Build_interpret.Static_deps.
        rule_deps
      ; action_deps
      } = Build_interpret.static_deps request ~all_targets:(load_dir t)
  in
  Pset.elements (Pset.union rule_deps action_deps)

let all_lib_deps t ~request =
  let targets = static_deps_of_request t request in
  List.fold_left (rules_for_targets t targets) ~init:Pmap.empty
    ~f:(fun acc rule ->
      let lib_deps = Build_interpret.lib_deps rule.Internal_rule.build in
      Pmap.merge acc lib_deps ~f:(fun _ a b ->
        match a, b with
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b
        | Some a, Some b -> Some (Build.merge_lib_deps a b)))

let all_lib_deps_by_context t ~request =
  let targets = static_deps_of_request t request in
  List.fold_left (rules_for_targets t targets) ~init:[] ~f:(fun acc rule ->
    let lib_deps = Build_interpret.lib_deps rule.Internal_rule.build in
    Path.Map.fold lib_deps ~init:acc ~f:(fun ~key:path ~data:lib_deps acc ->
      match Path.extract_build_context path with
      | None -> acc
      | Some (context, _) -> (context, lib_deps) :: acc))
  |> String_map.of_alist_multi
  |> String_map.map ~f:(function
    | [] -> String_map.empty
    | x :: l -> List.fold_left l ~init:x ~f:Build.merge_lib_deps)

module Rule = struct
  module Id = Internal_rule.Id

  type t =
    { id      : Id.t
    ; deps    : Path.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }

  let compare a b = Id.compare a.id b.id
end

module Rule_set = Set.Make(Rule)
module Id_set = Set.Make(Rule.Id)

let rules_for_files rules paths =
  List.fold_left paths ~init:Rule_set.empty ~f:(fun acc path ->
    match Pmap.find path rules with
    | None -> acc
    | Some rule -> Rule_set.add rule acc)
  |> Rule_set.elements

module Rule_closure =
  Top_closure.Make(Rule.Id)
    (struct
      type graph = Rule.t Pmap.t
      type t = Rule.t
      let key (t : t) = t.id
      let deps (t : t) (graph : graph) =
        rules_for_files graph (Pset.elements t.deps)
    end)

let build_rules ?(recursive=false) t ~request =
  let rules_seen = ref Id_set.empty in
  let rules = ref [] in
  let rec loop fn =
    match Hashtbl.find t.files fn with
    | None -> return ()
    | Some (File_spec.T { rule = ir; _ }) ->
      if Id_set.mem ir.id !rules_seen then
        return ()
      else begin
        rules_seen := Id_set.add ir.id !rules_seen;
        let rule =
          let make_rule rule_evaluation =
            rule_evaluation >>| fun (action, dyn_deps) ->
            { Rule.
              id      = ir.id
            ; deps    = Pset.union ir.static_deps dyn_deps
            ; targets = ir.targets
            ; context = ir.context
            ; action  = action
            }
          in
          match ir.exec with
          | Starting _ -> assert false (* guarded by [rules_seen] *)
          | Running { rule_evaluation; _ } | Evaluating_rule { rule_evaluation; _ } ->
            make_rule rule_evaluation
          | Not_started { eval_rule; exec_rule } ->
            ir.exec <- Starting { for_file = fn };
            let rule_evaluation =
              wrap_build_errors t ~targeting:fn ~f:eval_rule
            in
            ir.exec <-
              Evaluating_rule { for_file = fn
                              ; rule_evaluation
                              ; exec_rule
                              };
            make_rule rule_evaluation
        in
        rules := rule :: !rules;
        rule >>= fun rule ->
        if recursive then
          Future.all_unit (List.map (Pset.elements rule.deps) ~f:loop)
        else
          return ()
      end
  in
  let targets = ref Pset.empty in
  eval_request t ~request ~process_target:(fun fn ->
    targets := Pset.add fn !targets;
    loop fn)
  >>= fun () ->
  Future.all !rules
  >>| fun rules ->
  let rules =
    List.fold_left rules ~init:Pmap.empty ~f:(fun acc (r : Rule.t) ->
      Pset.fold r.targets ~init:acc ~f:(fun fn acc ->
        Pmap.add acc ~key:fn ~data:r))
  in
  match
    Rule_closure.top_closure rules
      (rules_for_files rules (Pset.elements !targets))
  with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle detected:\n   %s"
      (List.map cycle ~f:(fun rule -> Path.to_string (Pset.choose rule.Rule.targets))
       |> String.concat ~sep:"\n-> ")

(* +-----------------------------------------------------------------+
   | Adding rules to the system                                      |
   +-----------------------------------------------------------------+ *)

let rec add_build_dir_to_keep t ~dir =
  if not (Pset.mem dir t.build_dirs_to_keep) then begin
    t.build_dirs_to_keep <- Pset.add dir t.build_dirs_to_keep;
    let dir = Path.parent dir in
    if dir <> Path.root then
      add_build_dir_to_keep t ~dir
  end

let add_rule t (rule : Build_interpret.Rule.t) =
  let dir = rule.dir in
  match get_dir_status t ~dir with
  | Collecting_rules collector ->
    let rules = collectors.rules in
    if rules = [] && Path.is_in_build_dir dir then
      add_build_dir_to_keep t ~dir;
    collector.rules <- rule :: rules
  | Loaded _ ->
    Sexp.code_error "Build_system.add_rule called on closed directory"
      [ "dir", Path.sexp_of_t dir
      ; "targets", Sexp.To_sexp.list Path.sexp_of_t
                     (List.map rule.targets ~f:Build_interpret.Target.path)
      ]

let on_load_dir t ~dir ~f =
  match get_dir_status t ~dir with
  | Loaded _ ->
    Sexp.code_error "Build_system.on_dir_load called on closed directory"
      [ "dir", Path.sexp_of_t dir ]
  | Collecting_rules collector ->
    match collector.stage with
    | Loading -> f ()
    | Pending p ->
      let lazy_generators = p.lazy_generators in
      if lazy_generators = [] &&
         collector.rules = [] &&
         Path.is_in_build_dir dir then
        add_build_dir_to_keep t ~dir;
      p.lazy_generators <- f :: lazy_generators

let eval_glob t ~dir re =
  let targets = load_dir t ~dir |> Pset.elements |> List.map ~f:Path.basename in
  let files =
    match File_tree.find_dir t.file_tree dir with
    | None -> targets
    | Some d ->
      String_set.union (String_set.of_list targets) (File_tree.Dir.files d)
      |> String_set.elements
  in
  List.filter files ~f:(Re.execp re)

let targets_of = load_dir
let load_dir = load_dir_unit
