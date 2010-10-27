(*s: test_analyze_php.ml *)
open Common

module Ast = Ast_php

module Db = Database_php
module Cg = Callgraph_php

module V = Visitor_php

module Dp = Dataflow_pil

open OUnit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function ast_of_file *)
let ast_of_file file =
  Parse_php.parse_program file
(*e: function ast_of_file *)

let ast_of_file_safe file =
  let ast = ast_of_file file in
  if not (List.for_all (function
      Ast_php.NotParsedCorrectly _ -> false | _ -> true) ast)
  then failwith ("Parsing problem for: " ^ (Common.read_file file))
  else ast


(* It is clearer for our testing code to programmatically build source files
 * so that all the information about a test is in the same
 * file. You don't have to open extra files to understand the test
 * data.
 *)
let tmp_php_file_from_string s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file ("<?php\n" ^ s);
  tmp_file


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
      prj
  in
  db

(* Another shortcut *)
let db_from_string s =
  let tmp_file = tmp_php_file_from_string s in
  (* make sure it's a correct PHP file *)
  let ast = ast_of_file tmp_file in
  if not (List.for_all (function
           Ast_php.NotParsedCorrectly _ -> false | _ -> true) ast)
  then failwith ("Parsing problem for: " ^ s);
  db_of_files_or_dirs [tmp_file]


let db_from_fake_files xs =
  (* todo? would be better to create each time a fresh new dir *)
  let tmp_dir = "/tmp/pfff_fake_dir" in
  Common.command2 ("rm -rf " ^ tmp_dir);
  xs |> List.iter (fun (file, s) ->
    let dir = Filename.dirname file in
    Common.command2 (spf "mkdir -p %s/%s" tmp_dir dir);
    Common.write_file ~file:(Filename.concat tmp_dir file) ("<?php\n" ^ s);
  );
  let db = db_of_files_or_dirs [tmp_dir] in
  let db = { db with
    Database_php.project = Database_php.Project (tmp_dir, None);
  }
  in
  (* Common.command2 ("rm -rf " ^ tmp_dir); *)
  db

(*****************************************************************************)
(* Subsystem testing, no db *)
(*****************************************************************************)

let test_type_php file =
  let asts = ast_of_file file in

  let env = ref (Hashtbl.create 101) in
  let asts = asts +> List.map (fun ast ->
      Typing_php.annotate_toplevel env ast
    )
  in

  Sexp_ast_php.show_expr_info := true;
  pr (Sexp_ast_php.string_of_program asts);
  ()

let test_typing_weak_php file =
  let asts = ast_of_file file in
  asts +> List.iter (fun ast ->
    let xs = Typing_weak_php.extract_fields_per_var ast in
    pr2_gen xs
  )

let test_check_php file =
  raise Todo

let test_scope_php file =
  let asts = ast_of_file file in

  Check_variables_php.check_and_annotate_program
    ~find_entity:None
    asts;

  Sexp_ast_php.show_expr_info := true;
  pr (Sexp_ast_php.string_of_program asts);
  ()


let test_idl_to_php file =
  let asts = ast_of_file file in
  let idl_entries =
    Builtins_php.ast_php_to_idl asts
  in
  idl_entries +> List.iter (fun idl ->
    let s = Builtins_php.idl_entry_to_php_fake_code idl in
    pr s
  )

let test_visit2_php file =
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let hooks = { Visitor2_php.default_visitor with

    Visitor_php.klvalue = (fun (k, vx) e ->
      match fst e with
      | Ast_php.FunCallSimple (callname, args) ->
          let s = Ast_php.name callname in
          pr2 ("calling: " ^ s);

      | _ -> k e
    );
  } in
  let visitor = Visitor2_php.mk_visitor hooks in
  ast +> List.iter visitor.Visitor2_php.vorigin.Visitor_php.vtop


let test_xdebug_dumpfile file =
  file +> Xdebug.iter_dumpfile (fun acall ->
    (* pr2 s *)
    ()
  )

let test_parse_phpunit_json file =

  let json = Json_in.load_json file in
  let tr = Phpunit.test_results_of_json json in
  Phpunit.final_report tr

let test_php_xdebug file =
  let trace_file = Common.new_temp_file "xdebug" ".xt" in
  let php = Xdebug.php_cmd_with_xdebug_on ~trace_file () in
  let cmd = spf "%s %s" php file in
  pr2 (spf "executing: %s" cmd);
  Common.command2 cmd;
  trace_file +> Xdebug.iter_dumpfile ~show_progress:false (fun call ->
    let caller = call.Xdebug.f_call in
    let str = Callgraph_php.s_of_kind_call caller in
    let file = call.Xdebug.f_file in
    let line = call.Xdebug.f_line in
    pr (spf "%s:%d: %s" file line str);
  )



let test_type_xdebug_php file =
  let (d,b,e) = Common.dbe_of_filename file in
  assert(e = "php");
  let trace_file = Common.filename_of_dbe (d,b,"xt") in
  (* todo? remove pre-existing trace file ? because xdebug by default appends *)
  pr2 (spf "xdebug trace file in %s" trace_file);
  let cmd = Xdebug.php_cmd_with_xdebug_on ~trace_file () in
  let cmd = spf "%s %s" cmd file in
  pr2 (spf "executing: %s" cmd);
  Common.command2 cmd;

  let h = Hashtbl.create 101 in

  trace_file +> Xdebug.iter_dumpfile ~show_progress:true (fun call ->
    (* quite close to Database_php_build.index_db_xdebug *)

    let caller = call.Xdebug.f_call in
    let params = call.Xdebug.f_params in
    let ret = call.Xdebug.f_return in

    let str = Callgraph_php.s_of_kind_call caller in

    let tparams =
      params +> List.map Typing_trivial_php.type_of_expr in
    let tret =
      match ret with
      | None -> [Type_php.Unknown]
      | Some e -> Typing_trivial_php.type_of_expr e
    in
    let ft = [Type_php.Function (tparams +> List.map(fun t -> Some t), tret)] in

    h +> Common.hupdate_default str
      ~update:(fun old -> Typing_trivial_php.union_type old ft)
      ~default:(fun () -> ft);
  );
  h +> Common.hash_to_list +> List.iter (fun (s, t) ->
    pr2 (spf "%s -> %s" s (Type_php.string_of_phptype t));
  );
  ()

(*s: test_cfg_php *)
let test_cfg_php file =
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast |> List.iter (function
  | Ast_php.FuncDef def ->
      (try
        let flow = Controlflow_build_php.cfg_of_func def in
        Controlflow_php.display_flow flow;
      with Controlflow_build_php.Error err ->
        Controlflow_build_php.report_error err
      )
  | _ -> ()
  )

(*e: test_cfg_php *)
(*s: test_cyclomatic_php *)
let test_cyclomatic_php file =
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast |> List.iter (function
  | Ast_php.FuncDef def ->
      let name = Ast_php.name def.Ast_php.f_name in
      let n = Cyclomatic_php.cyclomatic_complexity_func ~verbose:true def in
      pr2 (spf "cyclomatic complexity for function %s is %d" name n);
  | Ast_php.ClassDef def ->
      let class_stmts = Ast_php.unbrace def.Ast_php.c_body in
      let class_name = Ast_php.name def.Ast_php.c_name in
      class_stmts |> List.iter (function
      | Ast_php.Method def ->
          let method_name = Ast_php.name def.Ast_php.m_name in
          let n = Cyclomatic_php.cyclomatic_complexity_method ~verbose:true def
          in
          pr2 (spf "cyclomatic complexity for method %s::%s is %d"
                  class_name method_name n);
      | Ast_php.ClassConstants _ | Ast_php.ClassVariables _ ->
          ()
      | Ast_php.XhpDecl _ ->
          ()
      )
  | _ -> ()
  )
(*e: test_cyclomatic_php *)

let test_dfg_php file =
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast |> List.iter (function
  | Ast_php.FuncDef def ->
      (try
        let flow = Controlflow_build_php.cfg_of_func def in
        let mapping = Dataflow_php_liveness.liveness_analysis flow in
        pr2_gen mapping
        (* Controlflow_php.display_flow flow; *)
      with Controlflow_build_php.Error err ->
        Controlflow_build_php.report_error err
      )
  | _ -> ()
  )

let test_pil file =
  let ast = Parse_php.parse_program file in

  (* let's transform and print every expression *)
  let hooks = { V.default_visitor with
    (* old:
    V.kexpr = (fun (k, vx) e ->
      let instrs = Pil_build.linearize_expr e in
      instrs +> List.iter (fun instr ->
        pr2 (Pil.string_of_instr instr);
      );
    );
    *)
    V.kstmt = (fun (k, vx) st ->
      let stmts = Pil_build.linearize_stmt st in
      stmts +> List.iter (fun st ->
        pr2 (Pil.string_of_stmt st)
      )
    );
  } in
  let v = V.mk_visitor hooks in
  v.V.vprogram ast

let test_pretty_print_pil file =
  let ast = Parse_php.parse_program file in
  let hooks = { V.default_visitor with
    V.kstmt = (fun (k, vx) st ->
      let stmts = Pil_build.linearize_stmt st in
      stmts +> List.iter (fun st ->
        pr2 (Pretty_print_pil.string_of_stmt st)
      )
    );
  } in
  let v = V.mk_visitor hooks in
  v.V.vprogram ast

let test_cfg_pil file =
  let ast = Parse_php.parse_program file in
  ast |> List.iter (function
  | Ast_php.FuncDef def ->
      (try
         let pil = Pil_build.linearize_body (Ast.unbrace def.Ast.f_body) in
         let flow = Controlflow_build_pil.cfg_of_stmts pil in
         Controlflow_pil.display_flow flow;
      with Controlflow_build_pil.Error err ->
        Controlflow_build_pil.report_error err
      )
  | _ -> ()
  )

let test_phpdoc dir =
  let files = Phpmanual_xml.find_functions_reference_of_dir dir in
  files +> List.iter (fun file ->
    let _func = Phpmanual_xml.function_name_of_xml_filename file in
    (* pr2 (spf "%s\n %s" func file); *)
    try
      let _xml = Phpmanual_xml.parse_xml file in
      ()
    with exn ->
      pr2 (spf "PB in %s" file);
  )

let test_dataflow_pil file =
  let ast = Parse_php.parse_program file in
  ast |> List.iter (function
  | Ast_php.FuncDef def ->
      (try
         let pil = Pil_build.linearize_body (Ast.unbrace def.Ast.f_body) in
         let flow = Controlflow_build_pil.cfg_of_stmts pil in
         let mp = Dp.reaching_fixpoint flow in
         Dp.display_reaching_dflow flow mp
      with Controlflow_build_pil.Error err ->
        Controlflow_build_pil.report_error err
      )
  | _ -> ()
  )

(*****************************************************************************)
(* Subsystem testing that requires a db *)
(*****************************************************************************)

let test_dependencies_php metapath =
  Database_php.with_db ~metapath (fun db ->
    Dependencies_php.dir_to_dir_dependencies db
  )

let test_function_pointer_analysis metapath =
  Database_php.with_db ~metapath (fun db ->

    (* move more code in aliasing_function_php.ml ? *)
    let h = Hashtbl.create 101 in

    Database_php_build.iter_files_and_topids db "FPOINTER" (fun id file ->
      let ast = db.Db.defs.Db.toplevels#assoc id in
      let funcvars = Lib_parsing_php.get_all_funcvars_ast ast in
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

let test_track_function_result function_name file =
  let db = db_of_files_or_dirs [file] in
  pr2 (spf "Tracking %s in %s" function_name file);
  let usage = Dataflow_php_array.track_function_result function_name db in
  Dataflow_php_array.print_usage usage;
  ()

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
  let xs = Htmlize_php.htmlize_pre file db in
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
(* Unit tests *)
(*****************************************************************************)

(*
 * Normally it would be better to put the unittest code in the unit tested
 * file, but sometimes for instance for the callgraph we actually need
 * a db which would add extra dependencies between directories
 * (e.g. foundation/ depending on database/). So better then to put
 * those unit tests here.
 *
 * Some of the tests depends on PHP files in tests/. An alternative that
 * would make the test code clearer would be to include those tests
 * files in ML but:
 *   - don't get the highlighting so no help if the test files contains
 *     bad PHP code
 *   - can not use the php interpreter on those files or xdebug or
 *     some of our command line tools like sgrep
 *)

(*---------------------------------------------------------------------------*)
(* Helpers *)
(*---------------------------------------------------------------------------*)

(* A few shortcuts to make our testing code more compact and declarative.
 * Allow for instance to get the id of a class by using the special :: syntax
 * as in (id "A::" db):
 *  - X::: for interface
 *  - X:: for a class
 *  - X::Y for a method
 *  - anything else for a function
 *)
let id s db =
  match s with
  | s when s =~ "\\([A-Za-z]+\\):::$" ->
      let (interface) = Common.matched1 s in
      Db.id_of_interface interface db

  | s when s =~ "\\([A-Za-z]+\\)::$" ->
      let (sclass) = Common.matched1 s in
      Db.id_of_class sclass db

  | s when  s =~ "\\([A-Za-z0-9_]+\\)::\\(.*\\)" ->
    let (sclass, smethod) = Common.matched2 s in
    Db.id_of_method sclass smethod db

  | _ ->
    Db.id_of_function s db

let callers id db =
  Db.callers_of_id id db |> List.map Cg.id_of_callerinfo
let callees id db =
  Db.callees_of_id id db |> List.map Cg.id_of_callsite


(*---------------------------------------------------------------------------*)
(* Callgraph *)
(*---------------------------------------------------------------------------*)

(*
 * One of the main feature of the code database (see database_php.mli) is
 * to store the full callgraph of some PHP code. This is used for instance
 * by the deadcode reaper to know which functions are never
 * called. Handling regular function calls is quite simple
 * but the semantic of PHP regarding methods, especially static
 * methods is not completely intuitive. Here are a few unit tests
 * to document this semantic and to get confidence in the callgraph
 * code which gets bigger.
 *)
let callgraph_unittest =
    "callgraph_php" >::: [

      (* Checking the call graph for code with simple function calls *)
      "simple function call" >:: (fun () ->
        let file = "
         function a() { b(); }
         function b() { }
         function z() { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let callees id = callees id db in

        assert_equal [id "a"] (callers (id "b"));
        assert_equal [id "b"] (callees (id "a"));
        assert_equal [] (callers (id "a"));
        assert_equal [] (callees (id "b"));
        assert_equal [] (callers (id "z"));
        assert_equal [] (callees (id "z"));
        assert_raises Not_found (fun () -> id "w");
      );

      (* Checking the semantic of static method calls. *)
      "simple static method call" >:: (fun () ->
        let file = "
          class A { static function a() { } }
          function b() { A::a(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let callees id = callees id db in
        assert_equal [id "A::a"] (callees (id "b"));
        assert_equal [id "b"] (callers (id "A::a"));
      );

      "static method call with self:: and parent::" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
           static function a2() { self::a(); }
          }
          class B extends A {
           function b() { parent::a(); }
          }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let _callees id = callees id db in
        assert_equal
          (sort [id "A::a2"; id "B::b"])
          (sort (callers (id "A::a")));
      );

      (* In PHP it is ok to call B::foo() even if B does not define
       * a static method 'foo' provided that B inherits from a class
       * that defines such a foo.
       *)
      "static method call and inheritance" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
          }
          class B extends A { }
          function c() { B::a(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        let callers id = callers id db in let _callees id = callees id db in
        assert_equal
          (sort [id "c"])
          (sort (callers (id "A::a")));
      );

      (* PHP is very permissive regarding static method calls as one can
       * do $this->foo() even if foo is a static method. PHP does not
       * impose the X::foo() syntax, which IMHO is just wrong.
       *)
      "static method call and $this" >:: (fun () ->
        let file = "
          class A {
           static function a() { }
           function a2() {
              $this->a();
          }
        }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let _id s = id s db in
        let _callers id = callers id db in let _callees id = callees id db in
        (* This currently fails, and I am not sure I want to fix it. Our
         * code should not use the $this->foo() syntax for static method
         * calls
         *
         * assert_equal
         * (sort [id "A::a2"])
         * (sort (callers (id "A::a")));
         *)
         ()
      );

      (* Checking method calls. *)
      "simple method call" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        Database_php_build.index_db_method db;
        (* shortcuts *)
        let id s = id s db in
        let _callers id = callers id db in let callees id = callees id db in
        assert_equal
         (sort [id "A::foo"])
         (sort (callees (id "c")));
      );

      (* Right now the analysis is very simple and does some gross over
       * approximation. With a call like $x->foo(), the analysis consider
       * any method foo in any class as a viable candidate. Doing a better
       * job would require some class and data-flow analysis.
       * Once the analysis gets more precise, fix those tests.
       *)
      "method call approximation" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          class B {
           function foo() { }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        Database_php_build.index_db_method db;
        (* shortcuts *)
        let id s = id s db in
        let _callers id = callers id db in let callees id = callees id db in
        assert_equal
         (sort [id "A::foo"; id "B::foo"]) (* sad, should have only A::foo *)
         (sort (callees (id "c")));
      );
    ]

(*---------------------------------------------------------------------------*)
(* Deadcode *)
(*---------------------------------------------------------------------------*)

(* The deadcode analysis in pfff we do for facebook not only find
 * dead code. It also:
 *  - generate patches to remove this code,
 *  - use blame information to know who wrote the code,
 *  - send code review request to diffcamp to the blamed person.
 *
 * Here we just want to unit test the basic functionality of the
 * deadcode reaper; assert that the reaper correctly find the
 * ids of the appropriate dead functions, or methods, or even classes.
 *)
let deadcode_unittest =
    "deadcode_php" >::: (

      (* the tests data is in pfff/tests/deadcode/. It consists of a few
       * small php files whose name, e.g. all_dead.php explains what
       * kind of function they contain.
       *)
      let deadcode_data_dir = Config.path ^ "/tests/php/deadcode/" in

      (* The deadcode analysis can be customized via "hooks".
       * This default hook is good enough for our unit tests.
       *)
      let hooks = Deadcode_php.default_hooks in

      [
      "simple dead functionss" >:: (fun () ->

        let db = db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db |> List.map snd in

        assert_bool
          "nocaller() should be dead"
          (List.mem (id "nocaller") dead_ids);

        assert_bool
          "file2_foo() should not be dead"
          (not (List.mem (id "file2_foo") dead_ids));

        (* pfff can parse XHP files by default now *)
        assert_bool
          "xhp_dead() should be dead, pfff should handle XHP code"
          (List.mem (id "xhp_dead") dead_ids);
      );

      (* With a fixpoint, the deadcode reaper can find more dead functions. *)
      "transitive dead functions" >:: (fun () ->

        let db = db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in
        let dead_ids =
          Deadcode_php.deadcode_fixpoint_per_file dead_ids hooks db +>
            Deadcode_php.ungroup_ids_by_file
        in
        assert_bool
          "calledbynocaller() should be dead with a fixpoint analysis"
          (List.mem (id "calledbynocaller") dead_ids);

        assert_bool
          "nocaller() should still be dead, even with a fixpoint analysis"
          (List.mem (id "nocaller") dead_ids);
      );

      (* Now that the callgraph understands static method calls, we can
       * not only find dead functions but also dead static methods.
       * See the different tests/deadcode/static_function*.php which
       * shows the subtelities of the semantic of static calls.
       *)
      "dead static methods" >:: (fun () ->
        let db = db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in

        assert_bool
          "StaticFunction::is_dead() should be dead"
          (List.mem (id "StaticFunction::is_dead") dead_ids);

        assert_bool
          "StaticFunction::not_dead() should not be dead"
          (not (List.mem (id "StaticFunction::not_dead") dead_ids));

        (* This static method is called indirectly via one of its
         * inherited class. The callgraph should understand that.
         *)
        assert_bool
          "StaticFunctionBase2::not_dead() should not be dead"
          (not (List.mem (id "StaticFunctionBase2::not_dead") dead_ids));

        assert_bool
          "StaticFunctionBase2::is_dead() should be dead"
          ((List.mem (id "StaticFunctionBase2::is_dead") dead_ids));


        (* This static method is called via the self:: syntax *)
        assert_bool
          "SF3::not_dead() should not be dead"
          (not (List.mem (id "SF3::not_dead") dead_ids));

        assert_bool
          "SF4_A::not_dead() should not be dead"
          (not (List.mem (id "SF4_A::not_dead") dead_ids));
      );

      (* PHP allows ugly things like using the $this->static_call()
       * syntax whereas it should really be self::static_call()
       * This can confuse our callgraph, and because it is right now
       * use too much in our codebase, it is better to not report
       * such code as dead
       *)

      "dead static methods part2" >:: (fun () ->
        let db = db_of_files_or_dirs [deadcode_data_dir] in
        (* shortcut *)
        let id s = id s db in

        let dead_ids =
          Deadcode_php.finding_dead_functions hooks db +> List.map snd in

        (* TODO *)
        OUnit.skip_if true "need better callgraph, or deadcode hook";
        assert_bool
          "SFTHIS::not_dead() should not be dead"
          (not (List.mem (id "SFTHIS::not_dead") dead_ids));

      );

      (* TODO: dead classes, dead defines *)

    ])

(*---------------------------------------------------------------------------*)
(* Final suite *)
(*---------------------------------------------------------------------------*)

let unittest =
  "analyze_php" >::: [
    callgraph_unittest;
    Test_coverage_php.unittest;
    deadcode_unittest;

    "class analysis" >::: [

      "users of a class" >:: (fun () ->
        let file = "
          class A {
           function foo() { }
          }
          class B {
           function foo() { new A(); }
          }
          function c() { $a = new A(); $a->foo(); }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          (sort [id "B::foo"; id "c"])
          (sort (Db.class_users_of_id (id "A::") db));

      );

      "extenders of a class" >:: (fun () ->
        let file = "
          class A { }
          class B extends A { }
          class C { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          [id "B::"]
          (Db.class_extenders_of_id (id "A::") db);

        assert_equal
          []
          (Db.class_extenders_of_id (id "C::") db);
      );

      "implementers of interface" >:: (fun () ->
        let file = "
          interface A { }
          interface A2 { }
          class B implements A { }
          class C implements A2, A { }
        "
        in
        let db = db_from_string file in
        (* shortcuts *)
        let id s = id s db in
        assert_equal
          (sort [id "B::";id "C::"])
          (sort (Db.class_implementers_of_id (id "A:::") db));
      );
    ];

    "include_require" >::: (
      let env = {
        Env_php.global_arrays = Common.hash_of_list [
          "_SERVER", Common.hash_of_list [
            "PHP_ROOT", "/home/foo/www";
          ];
        ];
        Env_php.constants = Common.hash_of_list [];
        Env_php.globals = Common.hash_of_list [];
        Env_php.globals_specials = (fun s dir -> None);
      }
      in [
      (* I was previously using Filename.concat in include_require_php.ml
       * which generate paths like a/b//c/foo.php which is annoying
       * and can confuse some of our analysis. Check that no regression
       * on this issue.
       *)
      "resolving path" >:: (fun () ->
        let file = "
        require_once $_SERVER['PHP_ROOT'].'/lib/alerts/alerts.php';
        "
        in
        let tmpfile = tmp_php_file_from_string file in
        let ast = ast_of_file_safe tmpfile in
        let incs = Include_require_php.top_increq_of_program ast in
        match incs with
        | [(_inc_kind,_tok, incexpr)] ->
            let path =
              Include_require_php.resolve_path (env, "/") incexpr in
            assert_equal
              (Some "/home/foo/www/lib/alerts/alerts.php")
              path;

        | _ ->
            assert_failure
              "wrong number of elements returned by increq_of_program"
      );

      (* It is useful to know the set of files that directly or indirectly
       * include a file.
       *)
      "includees includers" >:: (fun () ->
        let data = [
          "a.php", "";
          "b.php", "include_once 'a.php';";
          "c.php", "include_once 'b.php';";
          "w.php", "";
          "z.php", "include_once 'c.php'; include_once 'w.php'; ";
        ]
        in
        let db = db_from_fake_files data in
        Database_php_build2.index_db_includes_requires None db;

        let p file = Db.readable_to_absolute_filename file db in

        let includers_a = Db.includers_rec_of_file (p "a.php") db in
        assert_equal
          (sort [p "b.php"; p "c.php"; p "z.php"])
          (sort includers_a);

        let includees_z = Db.includees_rec_of_file (p "z.php") db in
        assert_equal
          (sort [p "c.php"; p "b.php"; p "a.php"; p "w.php"])
          (sort includees_z);
      );
    ]);
  ]


(* printing not static include *)
let test_include_require file =
  let ast = ast_of_file file in

  let increqs = Include_require_php.top_increq_of_program ast in
  increqs |> List.iter (fun (inckind, tok, incexpr) ->
    match incexpr with
    | Include_require_php.SimpleVar _
    | Include_require_php.Other _ ->
        Lib_parsing_php.print_match [tok]
    | _ -> ()
  );
  ()


(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

(* Note that other files in this directory define some cmdline actions:
 *  - database_php_build.ml
 *
 *)

let actions () = [
    "-test_pil",  " <file>",
    Common.mk_action_1_arg test_pil;
    "-test_pretty_print_pil", " <file>",
    Common.mk_action_1_arg test_pretty_print_pil;
    "-cfg_pil",  " <file>",
    Common.mk_action_1_arg test_cfg_pil;
    "-dataflow_pil", " <file",
    Common.mk_action_1_arg test_dataflow_pil;

  (*s: test_analyze_php actions *)
    "-cfg_php",  " <file>",
    Common.mk_action_1_arg test_cfg_php;
  (*x: test_analyze_php actions *)
    "-cyclomatic_php", " <file>",
    Common.mk_action_1_arg test_cyclomatic_php;
  (*e: test_analyze_php actions *)

  "-type_php", " <file>",
  Common.mk_action_1_arg test_type_php;

  "-check_php", " <file>",
  Common.mk_action_1_arg test_check_php;

  "-scope_php", " <file>",
  Common.mk_action_1_arg test_scope_php;

  "-visit2_php", "   <file>",
    Common.mk_action_1_arg test_visit2_php;

  "-weak_php", " <file>",
  Common.mk_action_1_arg test_typing_weak_php;
  "-php_xdebug", " <file>",
  Common.mk_action_1_arg test_php_xdebug;
  "-type_xdebug_php", " <file>",
  Common.mk_action_1_arg test_type_xdebug_php;

  "-dfg_php",  " <file>",
    Common.mk_action_1_arg test_dfg_php;

  "-idl_to_php", " <file>",
  Common.mk_action_1_arg test_idl_to_php;

  "-deadcode_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_deadcode_php;
  "-callgraph_php", " <files_or_dirs>",
  Common.mk_action_n_arg test_callgraph_php;


  "-test_track_function_result", " <function> <db>",
  Common.mk_action_2_arg (test_track_function_result);

  "-test_caller_rank", "<db>",
  Common.mk_action_1_arg (test_caller_rank);
  "-test_code_rank", "<db>",
  Common.mk_action_1_arg (test_code_rank);

  "-test_phpdoc", " <dir>",
  Common.mk_action_1_arg test_phpdoc;

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

  "-parse_xdebug_dumpfile", " <dumpfile>",
  Common.mk_action_1_arg test_xdebug_dumpfile;

  "-parse_phpunit_json", " <jsonfile>",
  Common.mk_action_1_arg test_parse_phpunit_json;

  "-include_require_static", " <file>",
  Common.mk_action_1_arg test_include_require;


]

(*e: test_analyze_php.ml *)
