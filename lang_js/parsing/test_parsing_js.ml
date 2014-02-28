open Common

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
  let fullxs =
    Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false xs
  in
  let stat_list = ref [] in

  fullxs +> Console.progress (fun k -> List.iter (fun file ->
    k();

    if file =~ ".*/third_party" || file =~ ".*/wiki/extensions"
    then pr2_once "IGNORING third party directory, bad unicode chars"
    else begin
      let (_xs, stat) =
      Common.save_excursion Flag.error_recovery true (fun () ->
      Common.save_excursion Flag.exn_when_lexical_error false (fun () ->
        Parse_js.parse file
      ))
      in
      Common.push stat stat_list;
    end
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()
(* see also:
 * git clone github.com/facebook/esprima
 * cd esprima/
 * git checkout fb-harmony
 * /home/engshare/third-party-tools/node/bin/node tools/generate-test-fixture.js "foo();"
 * /home/engshare/third-party-tools/node/bin/node tools/generate-test-fixture.js "foo();"
 *)
let test_dump_js file =
  let ast = Parse_js.parse_program file in
  let v = Meta_ast_js.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s

(*
let test_json_js file =
  let ast = Parse_js.parse_program file in
  let s = Export_ast_js.string_json_of_program ast in
  pr s;
  ()
*)
(*
let test_esprima file = 
  let json = Json_in.load_json file in
  let ast = Esprima.convert json in
  let v = Meta_ast_js.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s
*)

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

(*
  "-json_js", "   <file> export the AST of file into JSON",
  Common.mk_action_1_arg test_json_js;
  "-parse_esprima_json", " <file> ",
  Common.mk_action_1_arg test_esprima;
*)
]
