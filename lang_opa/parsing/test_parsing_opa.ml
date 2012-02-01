open Common

open Ast_opa
module Ast = Ast_opa
module Flag = Flag_parsing_opa

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_opa file = 
  if not (file =~ ".*\\.opa") 
  then pr2 "warning: seems not an OPA file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag.debug_lexer := true;

  let toks = Parse_opa.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_opa xs =

  let fullxs = Lib_parsing_opa.find_opa_files_of_dir_or_files xs in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    Common.save_excursion Flag.error_recovery true (fun () ->
      let _xs = Parse_opa.parse file in
      ()
    )
  );
  ()

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_opa", "   <file>", 
  Common.mk_action_1_arg test_tokens_opa;
  "-parse_opa", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_opa;
]
