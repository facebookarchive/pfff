(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module PI = Parse_info

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_eof = function
  | Parser_sql.EOF _ -> true
  | _ -> false

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file = 
  Common.with_open_infile file (fun chan ->
    let lexbuf = Lexing.from_channel chan in

    let rec aux acc = 
      let tok = Lexer_sql.lexer lexbuf in
      if is_eof tok
      then List.rev (tok::acc)
      else aux (tok::acc)
    in
    aux []
  )
let tokens a = 
  Common.profile_code "Parse_sql.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
  
let parse2 file = 
  Common.with_open_infile file (fun chan ->
    let lexbuf = Lexing.from_channel chan in

    try 
      let _expr = Parser_sql.main Lexer_sql.lexer lexbuf in
      ()
    with
    | Parsing.Parse_error -> 
        pr2 (Parse_info.error_message file (PI.lexbuf_to_strpos lexbuf))
  )

let parse a = 
  Common.profile_code "Parse_sql.parse" (fun () -> parse2 a)



let parse_string str = 
  let lexbuf = Lexing.from_string str in
  let expr = Parser_sql.main Lexer_sql.lexer lexbuf in
  expr
