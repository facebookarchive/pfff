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

let test_clang_dump file =
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

let test_clang_dump2 file =
  Common.cat file +> List.iter (fun s ->
    let str = Str.global_replace (Str.regexp ("\027\\[[^m]+m")) "" s in
    pr str
  )

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_clang", "   <file>", 
  Common.mk_action_1_arg test_tokens_clang;

  "-test_clang_dump", " <>",
  Common.mk_action_1_arg (test_clang_dump);
  "-test_clang_dump2", " <>",
  Common.mk_action_1_arg (test_clang_dump2);
]
