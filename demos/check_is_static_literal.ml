open Common

open Ast_php

module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The goal is to check that a string contains a PHP expression with only
 * literals (e.g. 1, 2,  array('a' => 1), etc).
 * 
 * We abuse the PHP parser which already contains such a category,
 * static_scalar, which is used when assigning a value to a class static
 * variables as in
 * 
 *   class A { static $x = <static_scalar_here>; }
 *)

let check_is_static_scalar s =
  let tmpfile = Common.new_temp_file "is_static" ".php" in
  Common.write_file ~file:tmpfile 
    (spf "<?php class A { static $x = %s; }" s);
  try 
    Flag_parsing_php.show_parsing_error := false;
    let ast = Parse_php.parse_program tmpfile in
    (* it is not enough to check that we can parse the string expression
     * when used in a static_scalar context. One could give
     * "1; } evil_stuff(); {" to close the current class and do evil_stuff
     * so here we check that the parse tree contains really only the
     * class and one static variable.
     *)
    match ast with
    (* copy paste of: pfff -dump_php tests/php/parsing/static_scalar.php *)
    | [ClassDef(
     {c_type= ClassRegular(i_2); c_name= Name(("A", i_3)); c_extends= None;
      c_implements= None; c_body=
     (i_4,
      [ClassVariables(VModifiers([(Static, i_5)]), None,
         [Left(
            (DName(("x", i_6)), Some((i_7, _static_scalar_here))))],
         i_9)],
      i_10);
      }); FinalDef(i_11)] ->
        true
    | _ -> 
        false

  with exn ->
    false

(*
let _ = example (check_is_static_scalar "1")
let _ = example (check_is_static_scalar "array('a' => 1)")
let _ = example (not (check_is_static_scalar "foo()"))
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* example: ./check_is_static_literal /tmp/scalar.php *)
let _ = 
  match (Array.to_list Sys.argv) with
  | [_; file] ->
      if check_is_static_scalar (Common.read_file file)
      then pr "ok"
      else pr "bad"
  | _ -> failwith "wrong number of arguments"
