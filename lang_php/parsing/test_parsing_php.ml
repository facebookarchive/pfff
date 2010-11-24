(*s: test_parsing_php.ml *)
open Common

open Ast_php
module Ast = Ast_php

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)
(*s: test_tokens_php *)
let test_tokens_php file = 
  if not (file =~ ".*\\.php") 
  then pr2 "warning: seems not a .php file";

  Flag_parsing_php.verbose_lexing := true;
  Flag_parsing_php.verbose_parsing := true;

  let toks = Parse_php.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

(*e: test_tokens_php *)
(*s: test_parse_php *)
let test_parse_php xs  =
  let ext = ".*\\.\\(php\\|phpt\\)$" in

  (* could now use Lib_parsing_php.find_php_files_of_dir_or_files *)
  let fullxs = Common.files_of_dir_or_files_no_vcs_post_filter ext xs in

  let stat_list = ref [] in
  (*s: initialize -parse_php regression testing hash *)
  let newscore  = Common.empty_score () in
  (*e: initialize -parse_php regression testing hash *)

  Common.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_php.parse file in

    Common.push2 stat stat_list;
    (*s: add stat for regression testing in hash *)
        let s = sprintf "bad = %d" stat.Parse_info.bad in
        if stat.Parse_info.bad = 0
        then Hashtbl.add newscore file (Common.Ok)
        else Hashtbl.add newscore file (Common.Pb s)
        ;
    (*e: add stat for regression testing in hash *)
  );

  Parse_info.print_parsing_stat_list !stat_list;
  (*s: print regression testing results *)
    let dirname_opt = 
      match xs with
      | [x] when is_directory x -> Some x
      | _ -> None
    in
    let score_path = Filename.concat Config.path "tmp" in
    dirname_opt +> Common.do_option (fun dirname -> 
      pr2 "--------------------------------";
      pr2 "regression testing  information";
      pr2 "--------------------------------";
      let str = Str.global_replace (Str.regexp "/") "__" dirname in
      Common.regression_testing newscore 
        (Filename.concat score_path
         ("score_parsing__" ^str ^ ext ^ ".marshalled"))
    );
    ()
  (*e: print regression testing results *)
(*e: test_parse_php *)
(*s: test_sexp_php *)
let test_sexp_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  (* let _ast = Type_annoter.annotate_program !Type_annoter.initial_env ast *) 

  Sexp_ast_php.show_info := false;
  let s = Sexp_ast_php.string_of_program ast in
  pr2 s;
  ()
(*x: test_sexp_php *)
let test_sexp_full_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  Sexp_ast_php.show_info := true;
  let s = Sexp_ast_php.string_of_program ast in
  pr2 s;
  ()
(*e: test_sexp_php *)
(*s: test_json_php *)
let test_json_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let s = Json_ast_php.string_of_program ast in
  pr s;
  ()

let test_json_fast_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let s = Json_ast_php.string_of_program_fast ast in
  pr s;
  ()
(*e: test_json_php *)

let test_dump_php file =
  let ast = Parse_php.parse_program file in
  let s = Export_ast_php.ml_pattern_string_of_program ast in
  pr s

(*s: test_visit_php *)
let test_visit_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let hooks = { Visitor_php.default_visitor with
    Visitor_php.kinfo = (fun (k, vx) info ->
      let s = Ast_php.str_of_info info in
      pr2 s;
    );

    Visitor_php.kexpr = (fun (k, vx) e -> 
      match fst e with
      | Ast_php.Sc x ->
          pr2 "scalar";
          k e
      | _ -> k e
    );
  } in
  let visitor = Visitor_php.mk_visitor hooks in
  visitor (Ast.Program ast)
(*e: test_visit_php *)

let test_unparse_php file = 
  let (ast2, stat) = Parse_php.parse file in
  let tmpfile = Common.new_temp_file "unparse_php" ".php" in
  let s = Unparse_php.string_of_program2_using_tokens ast2 in
  Common.write_file ~file:tmpfile s;
  let xs = Common.cmd_to_list (spf "diff -u %s %s" file tmpfile) in
  xs |> List.iter pr2;
  ()

(* note that pfff can now parse XHP files without calling xhpize *)
let test_parse_xhp_with_xhpize file = 
  let pp_cmd = "xhpize" in
  let (ast2, stat) = Parse_php.parse ~pp:(Some pp_cmd) file in
  let ast = Parse_php.program_of_program2 ast2 in
  Sexp_ast_php.show_info := false;
  let s = Sexp_ast_php.string_of_program ast in
  pr2 s;
  let s = Unparse_php.string_of_program2_using_tokens ast2 in
  pr2 s;
  ()

let test_parse_xdebug_expr s = 
  let e = Parse_php.xdebug_expr_of_string s in
  Sexp_ast_php.show_info := false;
  let s = Sexp_ast_php.string_of_expr e in
  pr2 s;
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let assert_no_parser_error ast = 
  assert_bool "bad: have a NotParsedCorrectly" 
    (List.for_all (function NotParsedCorrectly _ -> false | _ -> true) ast);
  ()

let unittest =
  "parsing_php" >::: [

    (* The PHP parser does not return an exception when a PHP file contains
     * an error, to allow some form of error recovery by not stopping 
     * at the first mistake. Instead it returns a NotParsedCorrectly 
     * AST toplevel element for parts of the code that were not parsed.
     * Here we check that correctly formed code do not contain such 
     * NotParsedCorrectly element.
     *)
    "parsing regular code" >:: (fun () ->
      let ast = Parse_php.program_of_string "echo 1+2;" in
      assert_no_parser_error ast;
    );

    (* Check that bad code contain a NotParsedCorrectly element. *)
    "rejecting bad code" >:: (fun () ->
      Flag_parsing_php.show_parsing_error := false;
      let ast = Parse_php.program_of_string "echo 1+" in
      assert_bool "bad: should have a NotParsedCorrectly" 
        (List.exists (function NotParsedCorrectly _ -> true | _ -> false) ast)
    );

    (* The PHP parser now understand PHP code containing XHP elements.
     * In the past pfff would call a preprocessor before parsing a file. By
     * setting this preprocessor to "xhpize", the XHP command line 
     * preprocessor, we could then parse the regular preprocessed code.
     * Now pfff can directly parse XHP code.
     *)
    "parsing xhp code" >:: (fun () ->
      (* old: Flag_parsing_php.pp_default := Some "xhpize"; *)

      let ast = Parse_php.program_of_string "return <x:frag />;"  in
      assert_no_parser_error ast;

      let ast = Parse_php.program_of_string "return $this->foo()[2];"  in
      assert_no_parser_error ast;
    );


    (* XHP is mainly a preprocessor to allow embbeding HTML-like tags in
     * PHP. It also fixes some limitations of the original PHP grammar 
     * regarding array access. You can do foo()['fld'] in XHP, which is 
     * not allowed in PHP (for stupid reasons IMHO).
     * The pfff PHP parser does not handle XHP tags but can handle 
     * this syntactic sugar at least.
     *)
    "parsing xhp fn_idx sugar code" >:: (fun () ->

      let ast = Parse_php.program_of_string "return foo()[2];"  in
      assert_no_parser_error ast;

      (* If the rule is added in the wrong place in the grammar, then
       * the previous test will work but not this one.
       *)
      let _ast = Parse_php.program_of_string "return $this->foo()[2];"  in
      OUnit.skip_if true "grammar extension for XHP incomplete";
      (*assert_no_parser_error ast;*)
    );

    (* Check that the visitor implementation correctly visit all AST 
     * subelements, even when they are deep inside the AST tree (e.g. 
     * sub-sub expressions inside parenthesis).
     *)
    "visitor" >:: (fun () ->
      let ast = Parse_php.program_of_string "echo 1+2+(3+4);" in

      let cnt = ref 0 in
      (* This is very tricky. See docs/manual/Parsing_php.pdf section 
       * 2.1.2 for a tutorial on visitors in OCaml. *)
      let hooks = { Visitor_php.default_visitor with
        Visitor_php.kexpr = (fun (k, _) e ->
          match Ast.untype e with
          | Sc _ -> incr cnt
          | _ -> k e
        )
      }
      in
      let visitor = Visitor_php.mk_visitor hooks in
      visitor (Program ast);
      assert_equal 4 !cnt ;
    );

    "checking column numbers" >:: (fun () ->
      
      (* See bug reported by dreiss, because the lexer had a few todos
       * regarding objects. *)
      let e = Parse_php.expr_of_string "$o->foo" in
      match Ast.untype e with
      | Lv v ->
          (match Ast.untype v with
          | ObjAccessSimple (v, tok, name) ->
              let info = Ast.info_of_name name in
              assert_equal 4 (Ast.col_of_info info)
          | _ -> 
              assert_failure "not good AST"
          )
      | _ ->
          assert_failure "not good AST"
    );

  (* todo: 
   *  - unparser is identity when do no modif
   *  - ? sexp and json output
   *  - ? correctness of Ast (too many cases)
   *)


  ]



(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  (*s: test_parsing_php actions *)
    "-parse_php", "   <file or dir>", 
    Common.mk_action_n_arg test_parse_php;
  (*x: test_parsing_php actions *)
    "-visit_php", "   <file>", 
      Common.mk_action_1_arg test_visit_php;
  (*x: test_parsing_php actions *)
    (* an alias for -sexp_php *)
    "-dump_php", "   <file>", 
    Common.mk_action_1_arg test_dump_php;
    "-dump_php_sexp", "   <file>", 
      Common.mk_action_1_arg test_sexp_php;
    "-dump_php_ml", "   <file>", 
    Common.mk_action_1_arg test_dump_php;
  (*x: test_parsing_php actions *)
    "-sexp_php", "   <file>", 
      Common.mk_action_1_arg test_sexp_php;
  (*x: test_parsing_php actions *)
    (* an alias for -sexp_php *)
    "-dump_full_ast", "   <file>", 
      Common.mk_action_1_arg test_sexp_full_php;
  (*x: test_parsing_php actions *)
    (* an alias for -sexp_php *)
    "-json", "   <file> export the AST of file into JSON", 
      Common.mk_action_1_arg test_json_php;
    "-json_fast", "   <file> export the AST of file into a compact JSON", 
      Common.mk_action_1_arg test_json_fast_php;
  (*x: test_parsing_php actions *)
    "-tokens_php", "   <file>", 
    Common.mk_action_1_arg test_tokens_php;
  (*e: test_parsing_php actions *)

    "-unparse_php", "   <file>", 
    Common.mk_action_1_arg test_unparse_php;
    "-parse_xdebug_expr", "   <string>", 
    Common.mk_action_1_arg test_parse_xdebug_expr;
    "-parse_xhp_with_xhpize", "   <file>", 
    Common.mk_action_1_arg test_parse_xhp_with_xhpize;

    "-unittest_parsing", "   ", 
    Common.mk_action_0_arg (fun () -> OUnit.run_test_tt unittest |> ignore);
]
(*e: test_parsing_php.ml *)
