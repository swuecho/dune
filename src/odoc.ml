open Import
open Jbuild
open Build.O

module SC = Super_context

let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

let module_deps (m : Module.t) ~dir ~dep_graph ~modules =
  Build.dyn_paths
    (dep_graph
     >>^ fun graph ->
     List.map (Utils.find_deps ~dir graph m.name)
       ~f:(fun name ->
         let m = Utils.find_module ~dir modules name in
         Module.odoc_file m ~dir))

let compile_module sctx (m : Module.t) ~odoc ~dir ~includes ~dep_graph ~modules
      ~lib_unique_name =
  let context = SC.context sctx in
  let odoc_file = Module.odoc_file m ~dir in
  SC.add_rule sctx
    (module_deps m ~dir ~dep_graph ~modules
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[odoc_file]
       [ A "compile"
       ; Dyn (fun x -> x)
       ; A "-I"; Path dir
       ; As ["--pkg"; lib_unique_name]
       ; Dep (Module.cmti_file m ~dir)
       ]);
  (m, odoc_file)

let to_html sctx (m : Module.t) odoc_file ~doc_dir ~odoc ~dir ~includes
      ~lib_unique_name ~(lib : Library.t) =
  let context = SC.context sctx in
  let html_dir = doc_dir ++ lib_unique_name ++ String.capitalize_ascii m.obj_name in
  let html_file = html_dir ++ "index.html" in
  SC.add_rule sctx
    (SC.Libs.static_file_deps (dir, lib) ~ext:odoc_ext
     >>>
     includes
     >>>
     Build.progn
       [ Build.remove_tree html_dir
       ; Build.mkdir html_dir
       ; Build.run ~context ~dir odoc ~extra_targets:[html_file]
           [ A "html"
           ; Dyn (fun x -> x)
           ; A "-I"; Path dir
           ; A "-o"; Path doc_dir
           ; Dep odoc_file
           ]
       ; Build.create_file (html_dir ++ Config.jbuilder_keep_fname)
       ]
    );
  html_file

let lib_index sctx ~odoc ~dir ~(lib : Library.t) ~lib_name ~lib_unique_name ~doc_dir ~modules
      ~includes =
  let context = SC.context sctx in
  let generated_index_mld = dir ++ sprintf "%s-generated.mld" lib.name in
  let source_index_mld = dir ++ sprintf "%s.mld" lib.name in
  let header = "{{: ../index.html} Up}" in
  SC.add_rule sctx
    (Build.if_file_exists source_index_mld
       ~then_:(Build.contents source_index_mld
               >>^ fun s -> sprintf "%s\n%s" header s)
       ~else_:(Build.arr (fun () ->
         (if lib.wrapped then
            sprintf
              "%s\n\
               {2 Library %s}\n\
               The entry point for this library is module {!module:%s}."
              header
              lib_name
              (String.capitalize_ascii lib.name)
          else
            sprintf
              "%s\n\
               {2 Library %s}\n\
               This library exposes the following toplevel modules:\n{!modules:%s}"
              header
              lib_name
              (String_map.keys modules |> String.concat ~sep:" "))))
     >>>
     Build.write_file_dyn generated_index_mld);
  let html_file =
    doc_dir ++ lib_unique_name ++ "index.html"
  in
  SC.add_rule sctx
    (SC.Libs.static_file_deps (dir, lib) ~ext:odoc_ext
     >>>
     includes
     >>>
     Build.run ~context ~dir odoc ~extra_targets:[html_file]
       [ A "html"
       ; Dyn (fun x -> x)
       ; A "-I"; Path dir
       ; A "-o"; Path doc_dir
       ; A "--index-for"; A lib_unique_name
       ; Dep generated_index_mld
       ]);
  html_file

let doc_dir ~context = Path.relative context.Context.build_dir "_doc"

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let toplevel_index ~doc_dir = doc_dir ++ "index.html"

let setup_library_rules sctx (lib : Library.t) ~dir ~modules ~requires
      ~(dep_graph:Ocamldep.dep_graph) =
  let lib_unique_name = SC.unique_library_name sctx (Internal (dir, lib)) in
  let lib_name = Library.best_name lib in
  let context = SC.context sctx in
  let dep_graph =
    Build.memoize "odoc deps"
      ((* Use the dependency graph given by ocamldep. However, when a module has no
          .mli, use the dependencies for the .ml *)
        Build.fanout dep_graph.intf dep_graph.impl
        >>^ fun (intf, impl) ->
        String_map.merge intf impl ~f:(fun _ intf impl ->
          match intf, impl with
          | Some _, _    -> intf
          | None, Some _ -> impl
          | None, None -> assert false))
  in
  let odoc = get_odoc sctx in
  let includes =
    Build.memoize "includes"
      (requires
       >>>
       SC.Libs.file_deps sctx ~ext:odoc_ext
       >>^ Lib.include_flags)
  in
  let modules_and_odoc_files =
    List.map (String_map.values modules)
      ~f:(compile_module sctx ~odoc ~dir ~includes ~dep_graph ~modules
            ~lib_unique_name)
  in
  SC.Libs.setup_file_deps_alias sctx ~ext:odoc_ext (dir, lib)
    (List.map modules_and_odoc_files ~f:snd);
  let doc_dir = doc_dir ~context in
    (*
     let modules_and_odoc_files =
     if lib.wrapped then
     let main_module_name = String.capitalize_ascii lib.name in
     List.filter modules_and_odoc_files
     ~f:(fun (m, _) -> m.Module.name = main_module_name)
     else
     modules_and_odoc_files
       in*)
  let html_files =
    List.map modules_and_odoc_files ~f:(fun (m, odoc_file) ->
      to_html sctx m odoc_file ~doc_dir ~odoc ~dir ~includes ~lib
        ~lib_unique_name)
  in
  let lib_index_html =
    lib_index sctx ~dir ~lib ~lib_unique_name ~lib_name ~doc_dir
      ~modules ~includes ~odoc
  in
  Alias.add_deps (SC.aliases sctx) (Alias.doc ~dir)
    (css_file ~doc_dir
     :: toplevel_index ~doc_dir
     :: lib_index_html
     :: html_files)

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = doc_dir ~context in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[css_file ~doc_dir]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ])

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let list_items =
    Super_context.stanzas_to_consider_for_install sctx
    |> List.filter_map ~f:(fun (_path, stanza) ->
      match stanza with
      | Stanza.Library
          {Library.kind = Library.Kind.Normal; public = Some public_info; _} ->
        let name = public_info.name in
        let link = sp {|<a href="%s/index.html">%s</a>|} name name in
        let version_suffix =
          match public_info.package.Package.version_from_opam_file with
          | None ->
            ""
          | Some v ->
            sp {| <span class="version">%s</span>|} v
        in
        Some (sp "<li>%s%s</li>" link version_suffix)

      | _ ->
        None)
  in
  let list_items = String.concat ~sep:"\n    " list_items in
  let html =
    sp {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <div class="by-name">
    <h2>OCaml package documentation</h2>
    <ol>
    %s
    </ol>
 </body>
 </html>
|} list_items
  in
  let context = SC.context sctx in
  let doc_dir = doc_dir ~context in
  SC.add_rule sctx @@ Build.write_file (toplevel_index ~doc_dir) html

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | lib :: _ ->
    match Lib_db.find (SC.libs sctx) ~from:dir lib with
    | None | Some (External _) -> ()
    | Some (Internal (dir, _)) -> SC.load_dir sctx ~dir
