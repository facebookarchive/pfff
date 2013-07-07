open Common
open OUnit

open Ast_js
module Ast = Ast_js
module Flag = Flag_parsing_js

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_js file = 
  if not (file =~ ".*\\.js") 
  then pr2 "warning: seems not a .js file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_js.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_js xs  =
  let ext = ".*\\.\\(js\\|javascript\\)$" in

  (* could now use Lib_parsing_php.find_php_files_of_dir_or_files *)
  let fullxs = Common2.files_of_dir_or_files_no_vcs_post_filter ext xs in

  let stat_list = ref [] in

  Common2.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    if file =~ ".*/third_party" || file =~ ".*/wiki/extensions"
    then pr2 "IGNORING third party directory, bad unicode chars"
    else begin

      let (xs, stat) = Parse_js.parse file in
      Common.push2 stat stat_list;
    end
  );
  Parse_info.print_parsing_stat_list !stat_list;
  ()


let test_json_js file = 
  let ast = Parse_js.parse_program file in
  let s = Export_ast_js.string_json_of_program ast in
  pr s;
  ()

let test_dump_js file =
  let ast = Parse_js.parse_program file in
  let s = Export_ast_js.ml_pattern_string_of_program ast in
  pr s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_js", "   <file>", 
  Common.mk_action_1_arg test_tokens_js;
  "-parse_js", "   <file or dir>", 
  Common.mk_action_n_arg test_parse_js;
  "-dump_js", "   <file>", 
  Common.mk_action_1_arg test_dump_js;

  "-json", "   <file> export the AST of file into JSON", 
  Common.mk_action_1_arg test_json_js;
]
