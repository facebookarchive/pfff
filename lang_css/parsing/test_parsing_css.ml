open Common

open Ast_css
module Ast = Ast_css
module Flag = Flag_parsing_css

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_css file = 
  if not (file =~ ".*\\.css") 
  then pr2 "warning: seems not a css file";

(*
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  let (_ast, toks) = Parse_html.parse file in
  toks +> List.iter (fun x -> pr2_gen x);
*)
  ()

let test_parse_css xs =

  let fullxs = Lib_parsing_css.find_css_files_of_dir_or_files xs in
  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);
    ()
  );
  ()

let test_dump_css file =
  raise Todo

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_css", "   <file>", 
  Common.mk_action_1_arg test_tokens_css;
  "-parse_css", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_css;
  "-dump_css", "   <file>", 
  Common.mk_action_1_arg test_dump_css;
]
