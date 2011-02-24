open Common

open Ast_html
module Ast = Ast_html
module Flag = Flag_parsing_html

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_html file = 
  if not (file =~ ".*\\.html") 
  then pr2 "warning: seems not a html file";

  raise Todo
(*
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_erlang.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()
*)

let test_parse_html xs =

  let fullxs = Lib_parsing_html.find_html_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    raise Todo
    (*
    let (xs, stat) = Parse_erlang.parse file in
    Common.push2 stat stat_list;
    *)
  );
  Parse_info.print_parsing_stat_list !stat_list;
  ()

let test_dump_html file =
  raise Todo
(*
  let ast = Parse_html.parse_program file in
  let s = Export_ast_ml.ml_pattern_string_of_program ast in
  pr s
*)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_html", "   <file>", 
  Common.mk_action_1_arg test_tokens_html;
  "-parse_html", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_html;
  "-dump_html", "   <file>", 
  Common.mk_action_n_arg test_dump_html;
]
