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

let test_parse_ml_or_mli xs =

  let fullxs = Lib_parsing_ml.find_ml_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_ml.parse file in
    Common.push2 stat stat_list;
  );
  Parse_info.print_parsing_stat_list !stat_list;
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
  "-parse_ml", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_ml_or_mli;
]
