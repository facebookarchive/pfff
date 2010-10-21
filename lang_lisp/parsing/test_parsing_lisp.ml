open Common

open Ast_lisp
module Ast = Ast_lisp
module Flag = Flag_parsing_lisp

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_lisp file = 
  if not (file =~ ".*\\.nw") 
  then pr2 "warning: seems not a noweb file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_lisp.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_lisp", "   <file>", 
  Common.mk_action_1_arg test_tokens_lisp;
]
