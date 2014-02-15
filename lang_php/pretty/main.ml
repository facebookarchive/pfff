
(*****************************************************************************)
(* Pretty-printer Main *)
(*****************************************************************************)

(* Main entry point for the pretty printer *)
(* Takes in php files, spits out their pretty printed version on stdout *)

let usage _s =
  Printf.fprintf stderr "Usage: %s files\n" Sys.argv.(0);
  exit 2

let make_tmp() =
  let fn = Filename.temp_file "" ".php" in
  let oc = open_out fn in
  try
    while true do
      output_string oc (read_line());
      output_string oc "\n"
    done;
    assert false
  with End_of_file ->
    close_out oc;
    fn

let print_token tok =
  let inf = Token_helpers_php.info_of_tok tok in
  let tok = Parse_info.str_of_info inf in
  let line = Parse_info.line_of_info inf in
  Printf.printf "/BEGIN[%d]:%s:END/\n" line tok

let () =
  let files = List.tl (Array.to_list Sys.argv) in
  let files = if files = [] then [make_tmp()] else files in
  List.iter (
  fun file ->
    let tokens = Parse_php.tokens file in
(*     List.iter print_token tokens; if true then exit 0; *)
    let ast    = Parse_php.parse_program file in
    let ast    = Ast_pp_build.program_with_comments tokens ast in
    Pretty_print.program print_string ast
 ) files
