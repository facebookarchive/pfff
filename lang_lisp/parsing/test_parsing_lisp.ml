open Common

open Ast_lisp
module Ast = Ast_lisp
module Flag = Flag_parsing_lisp

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_lisp file = 
  (match File_type.file_type_of_file file with
  | File_type.PL (File_type.Lisp _) -> ()
  | _ -> pr2 "warning: seems not a lisp file";
  );

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
