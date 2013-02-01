open Common

open Ast_clang
module Ast = Ast_clang
module Flag = Flag_parsing_clang

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_clang file = 
  if not (file =~ ".*\\.clang") 
  then pr2 "warning: seems not a clang file";

  let toks = Parse_clang.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_clang", "   <file>", 
  Common.mk_action_1_arg test_tokens_clang;
]
