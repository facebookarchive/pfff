
module A = Ast_php_simple
module Infer = Typing_php


let () =
  let file = Sys.argv.(1) in
  Printf.printf "Request %s (%s): " file "wtf?"; flush stdout;
  let ast = Parse_php.parse_program file in
  Printf.printf "Parsed\n"; flush stdout;
  let ast = Ast_php_simple_build.program ast in
  Printf.printf "Built\n"; flush stdout;
  let env = Env_typing_php.make_env () in
  Infer.stmtl env ast; (*Type info in env*)
  failwith "TOFIX"
(*
  let at = Typing_helpers_php.Array_typer.make_array_typer "bogus file" in
  Typing_helpers_php.Array_typer.tym env at;

  Typing_helpers_php.Array_typer.pp at;
  Typing_helpers_php.Print2.genv env;
  Typing_helpers_php.Print2.arr_structure env;
  (*Typing_helpers_php.Print2.aenv env;*)
  (*Typing_helpers_php.Print2.print_arr_info env;*)
*)
