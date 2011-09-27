(*s: test_analyze_db_php.ml *)
open Common

module Ast = Ast_php

module Db = Database_php
module Cg = Callgraph_php

module V = Visitor_php

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* The default way to analyze a set of PHP files is to first build
 * a database containing information about the code (stored internally
 * using Berkeley DB), e.g. with ./pfff_db ~/www -metapath /tmp/pfff_db,
 * and then run different analysis on this database, e.g. with
 * ./pfff_misc -deadcode_analysis /tmp/pfff_db.
 * In our testing code we want to test some of our analysis without
 * requiring to have a directory with a set of files, or some space on
 * disk to store the database. This small wrapper allows to build
 * a database in memory from a give set of files, usually temporary
 * files built with tmp_php_file_from_string() below.
 *)
let db_of_files_or_dirs files_or_dirs =

  (* prj is normally used in GUI to display files relative to a specific
   * project base. Here we want to analyze a set of adhoc files or multiple
   * dirs so there is no base so we use /
   *)
  let prj = Database_php.Project ("/", None) in

  let php_files =
    Lib_parsing_php.find_php_files_of_dir_or_files files_or_dirs
    |> List.map Common.relative_to_absolute
  in
  let db =
    Database_php_build.create_db
     ~db_support:Database_php.Mem
     ~files:(Some php_files)
     ~annotate_variables_program:
      (Some Check_variables_php.check_and_annotate_program)
     prj
  in
  db

(*****************************************************************************)
(* Subsystem testing that requires a db *)
(*****************************************************************************)

let test_dependencies_php metapath =
  Database_php.with_db ~metapath (fun db ->
    Dependencies_php.dir_to_dir_dependencies db
  )

let test_function_pointer_analysis metapath =
  raise Todo
(*
  Database_php.with_db ~metapath (fun db ->

    (* move more code in aliasing_function_php.ml ? *)
    let h = Hashtbl.create 101 in

    Database_php_build.iter_files_and_topids db "FPOINTER" (fun id file ->
      let ast = db.Db.defs.Db.toplevels#assoc id in
      let funcvars = Lib_parsing_php.get_funcvars_any (Ast.Toplevel ast) in
      funcvars +> List.iter (fun dvar ->
        pr2 dvar;
        let prefixes =
          Aliasing_function_php.finding_function_pointer_prefix dvar ast in
        prefixes +> List.iter (fun s ->
          pr2(spf " '%s'" s);
          Hashtbl.replace h s true;
        );
      )
    );
    pr2 "dangerous prefixes:";
    h +> Common.hashset_to_list +> List.iter pr2;
  )
*)


let test_deadcode_php files_or_dirs =
  (* create database of code information, used by our deadcode global
   * analysis below
   *)
  let db = db_of_files_or_dirs files_or_dirs in

  let hooks_deadcode = { Deadcode_php.default_hooks with
    Deadcode_php.print_diff = true;
  } in
  let dead =
    Deadcode_php.finding_dead_functions hooks_deadcode db
  in
  pr_xxxxxxxxxxxxxxxxx();
  pr "Dead functions:";
  pr_xxxxxxxxxxxxxxxxx();
  dead +> List.iter (fun (s, id) ->
    pr (spf "%s at %s" s (Database_php.str_of_id id db));
  );

  Deadcode_php.deadcode_analysis hooks_deadcode db

let test_callgraph_php files_or_dirs =

  let db = db_of_files_or_dirs files_or_dirs in

  (* converting the callgraph stored as two assocs in the db
   * into a ograph_mutable that can be displayed with gv.
   *)
  let g = new Ograph_simple.ograph_mutable in

  db.Db.fullid_of_id#iter (fun (id, fullid) ->
    let node = Db.name_of_id id db in
    g#add_node id node
  );

  db.Db.fullid_of_id#iter (fun (id, _) ->
    try
      let callsites = Db.callees_of_id id db in
      callsites |> List.iter (fun (Callgraph_php.CallSite (id2, kind_call)) ->
        g#add_arc (id, id2) ();
      )
    with
     (* class id have no callees *)
     _ -> ()
  );
  Ograph_simple.print_ograph_generic
    ~str_of_key:(fun id ->
      let (Entity_php.Id i) = id in
      i_to_s i
    )
    ~str_of_node:(fun id node -> node)
    "/tmp/test_callgraph.dot"
    g;
  ()


(* topological sort of strongly connected components *)
let test_topo_sorted_strongly_connected_callgraph_php files_or_dirs =

  let db = db_of_files_or_dirs files_or_dirs in
  let str_of_key id = Db.complete_name_of_id id db in

  (* converting the callgraph stored as two assocs in the db
   * into something algorithms in ocamlgraph/ can work on
   *)
  let g = Graph_php.build_simple_callgraph db in
  Graph_php.display_with_gv g db;

  let scc, hscc = Graph.strongly_connected_components g in
  Graph.display_strongly_connected_components ~str_of_key hscc g;

  ()


let test_track_function_result function_name file =
  raise Todo
(*
  let db = db_of_files_or_dirs [file] in
  pr2 (spf "Tracking %s in %s" function_name file);
  let usage = Dataflow_php_array.track_function_result function_name db in
  Dataflow_php_array.print_usage usage;
  ()
*)

(*---------------------------------------------------------------------------*)
(* Code rank stuff *)
(*---------------------------------------------------------------------------*)
let test_caller_rank metapath =
  Database_php.with_db ~metapath (fun db ->
    let res = Code_rank_php.build_naive_caller_ranks db in
    let xs = res.Code_rank_php.function_ranks#tolist in
    let sorted = Common.sort_by_val_lowfirst xs in

    sorted +> List.iter (fun (id, v) ->
      let s = Db.name_of_id id db in
      pr2 (spf "%s : %f" s v);
    );

  )
let test_code_rank metapath =
  Database_php.with_db ~metapath (fun db ->

    let res = Code_rank_php.build_code_ranks db in
    let xs = res.Code_rank_php.function_ranks#tolist in
    let sorted = Common.sort_by_val_lowfirst xs in

    sorted +> List.iter (fun (id, v) ->
      let s = Db.name_of_id id db in
      pr2 (spf "%s : %f" s v);
    );

  )

(*---------------------------------------------------------------------------*)
(* Includers/includees *)
(*---------------------------------------------------------------------------*)
let test_includers_php metapath file _depth =
  Database_php.with_db ~metapath (fun db ->
    let file = Common.realpath file in
    let xs = Db.includers_rec_of_file file db in
    xs |> List.iter pr;
  )

let test_includees_php metapath file depth =
  Database_php.with_db ~metapath (fun db ->
    let file = Common.realpath file in
    (*
    let xs = Db.includees_rec_of_file file db in
      xs |> List.iter pr;
    *)
    let g = Db.includees_graph_of_file ~depth_limit:(Some depth) file db in
    Graph.print_graph_generic
      ~str_of_key:(fun file ->
        Db.absolute_to_readable_filename file db
      )
      "/tmp/ocamlgraph.dot"
      g;
  )

(*---------------------------------------------------------------------------*)
(* Code highlighting *)
(*---------------------------------------------------------------------------*)
let generate_html_php file =
  let file = Common.realpath file in
  let nblines = Common.cat file |> List.length in

  let db = db_of_files_or_dirs [file] in
  let xs = Htmlize_php.htmlize_pre 
    ~hook_token:(fun s tok categ -> XHTML2.M.pcdata s)
    file db in
  let xs' = Common.index_list_1 xs in
  xs' +> List.iter (fun (s, i) ->
    pr2 (spf "%d: %s" i s);
  );

  let nblines2 = List.length xs in

  if nblines2 <> nblines
  then failwith (spf "The number of lines differs, orig = %d <> %d"
                    nblines nblines2);
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [

  "-deadcode_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_deadcode_php;

  "-callgraph_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_callgraph_php;

  "-callgraph_topo_scc_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_topo_sorted_strongly_connected_callgraph_php;


  "-test_track_function_result", " <function> <db>",
  Common.mk_action_2_arg (test_track_function_result);

  "-test_caller_rank", "<db>",
  Common.mk_action_1_arg (test_caller_rank);
  "-test_code_rank", "<db>",
  Common.mk_action_1_arg (test_code_rank);


  "-dependencies_php", " <metapath>",
  Common.mk_action_1_arg test_dependencies_php;

  "-function_pointer_analysis", "<db>",
  Common.mk_action_1_arg (test_function_pointer_analysis);

  "-includers_php", "<db> <file> <depth>",
  Common.mk_action_3_arg (test_includers_php);
  "-includees_php", "<db> <file> <depth>",
  Common.mk_action_3_arg (fun db file depth ->
    test_includees_php db file (s_to_i depth));

  "-php_to_html", "<file>",
  Common.mk_action_1_arg (generate_html_php);


]

(*e: test_analyze_db_php.ml *)
