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

let test_parse_clang xs =

  let fullxs = Lib_parsing_clang.find_source_files_of_dir_or_files xs in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);
    let _ast = Parse_clang.parse file in
    ()
  );
  ()


let test_clang_split_dump file =
  let chan = open_in file in

  let chan_out = open_out "/dev/null" in
  let re_color = (Str.regexp ("\027\\[[^m]+m")) in
  
  let rec aux chan_out =
    let s, eof = 
      try 
        input_line chan, false
      with End_of_file -> "", true
    in
    if eof then ()
    else 
      if s =~ "^Processing: /Users/mathieubaudet/git/fbobjc/\\(.*\\)\\.$"
      then begin
        let file = Common.matched1 s in
        let file = Str.global_replace (Str.regexp " ") "___" file in
        let (d,b,e) = Common.dbe_of_filename file in
        pr2 file;
        Common.command2 (spf "mkdir -p %s" d);
        close_out chan_out;
        let file = Common.filename_of_dbe (d,b,"clang") in
        let chan_out = open_out file in
        aux chan_out
      end
      else begin
        let s = Str.global_replace re_color  "" s in
        output_string chan_out s;
        output_string chan_out "\n" ;
        aux chan_out
      end
  in
  aux chan_out

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_clang", "   <file>", 
  Common.mk_action_1_arg test_tokens_clang;
  "-parse_clang", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_clang;

  "-split_clang_dump", " <>",
  Common.mk_action_1_arg (test_clang_split_dump);
]
