open Common

open Ast_ml
module Ast = Ast_ml
module Flag = Flag_parsing_ml

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_ml file = 
  if not (file =~ ".*\\.ml[iyl]?") 
  then pr2 "warning: seems not a ocaml file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_ml.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_ml", "   <file>", 
  Common.mk_action_1_arg test_tokens_ml;
]
