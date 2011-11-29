(*s: test_parsing_php.ml *)
open Common

open Ast_php
module Ast = Ast_php

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

    let (xs, stat) = 
    Common.save_excursion Flag_parsing_php.error_recovery true (fun () ->
      Parse_php.parse file 
    )
    in

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

  Export_ast_php.show_info := false;
  let s = Export_ast_php.sexp_string_of_program ast in
  pr2 s;
  ()
(*x: test_sexp_php *)
let test_sexp_full_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  Export_ast_php.show_info := true;
  let s = Export_ast_php.sexp_string_of_program ast in
  pr2 s;
  ()
(*e: test_sexp_php *)
(*s: test_json_php *)
let test_json_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let s = Export_ast_php.json_string_of_program ast in
  pr s;
  ()

let test_json_fast_php file = 
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let s = Export_ast_php.json_string_of_program_fast ast in
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
      match e with
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
  let s = Unparse_php.string_of_program2_using_transfo ast2 in
  Common.write_file ~file:tmpfile s;
  let xs = Common.unix_diff file tmpfile in
  xs |> List.iter pr2;
  ()

let test_pretty_print_php file = 
  let (ast2, stat) = Parse_php.parse file in
  let _ast = Parse_php.program_of_program2 ast2 in
  raise Todo
  (* Pretty_print_php.pretty_print_program ast *)

(* note that pfff can now parse XHP files without calling xhpize *)
let test_parse_xhp_with_xhpize file = 
  let pp_cmd = "xhpize" in
  let (ast2, stat) = Parse_php.parse ~pp:(Some pp_cmd) file in
  let ast = Parse_php.program_of_program2 ast2 in
  Export_ast_php.show_info := false;
  let s = Export_ast_php.sexp_string_of_program ast in
  pr2 s;
  let s = Unparse_php.string_of_program2_using_transfo ast2 in
  pr2 s;
  ()

let test_parse_xdebug_expr s = 
  let e = Parse_php.xdebug_expr_of_string s in
  Export_ast_php.show_info := false;
  let s = Export_ast_php.sexp_string_of_expr e in
  pr2 s;
  ()

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
    "-json", "   <file> export the AST of file into JSON", 
      Common.mk_action_1_arg test_json_php;
    "-json_fast", "   <file> export the AST of file into a compact JSON", 
      Common.mk_action_1_arg test_json_fast_php;
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
    "-tokens_php", "   <file>", 
    Common.mk_action_1_arg test_tokens_php;
  (*e: test_parsing_php actions *)

    "-unparse_php", "   <file>", 
    Common.mk_action_1_arg test_unparse_php;
    "-pretty_print_php", "   <file>", 
    Common.mk_action_1_arg test_pretty_print_php;
    "-parse_xdebug_expr", "   <string>", 
    Common.mk_action_1_arg test_parse_xdebug_expr;
    "-parse_xhp_with_xhpize", "   <file>", 
    Common.mk_action_1_arg test_parse_xhp_with_xhpize;
]
(*e: test_parsing_php.ml *)
