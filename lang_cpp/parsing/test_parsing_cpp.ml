open Common

open Ast_cpp
module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cpp file = 
  if not (file =~ ".*\\.cpp") 
  then pr2 "warning: seems not a c++ file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_cpp.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

(*
let test_parse_js xs  =
  let ext = ".*\\.\\(js\\|javascript\\)$" in

  (* could now use Lib_parsing_php.find_php_files_of_dir_or_files *)
  let fullxs = Common.files_of_dir_or_files_no_vcs_post_filter ext xs in

  let stat_list = ref [] in

  Common.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    if file =~ ".*/third_party" || file =~ ".*/wiki/extensions"
    then pr2 "IGNORING third party directory, bad unicode chars"
    else begin

      let (xs, stat) = Parse_js.parse file in
      Common.push2 stat stat_list;
    end
  );
  Parse_js.print_parsing_stat_list !stat_list;
  ()

*)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)


(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [

(*
    "-parse_cpp", "   <file or dir>", 
    Common.mk_action_n_arg test_parse_cpp;
*)

    "-tokens_cpp", "   <file>", 
    Common.mk_action_1_arg test_tokens_cpp;

(*
    "-unparse_js", "   <file>", 
    Common.mk_action_1_arg test_unparse_js;

    "-json", "   <file> export the AST of file into JSON", 
      Common.mk_action_1_arg test_json_js;
*)
]

