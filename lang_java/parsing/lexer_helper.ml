(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
   Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

let file = ref ""

let line = ref 0

let set_file_name f =
  file := f;
  line := 1

let next_line buf =
  line := !line + 1

let location () = Printf.sprintf "file %s, line %d" !file !line

let lexeme_pos buf = Lexing.lexeme_start buf

type comment = { mutable buffer : string; mutable pos : int }

type comments = comment list

let comment_list = ref []

let comments () = List.rev !comment_list

let new_comment () =
  let com = { buffer = ""; pos = -1 } in
  comment_list := com :: !comment_list;
  com

let current_comment () = List.hd !comment_list

let begin_comment buf =
  let comment = new_comment () in
  comment.pos <- lexeme_pos buf;
  comment.buffer <- Lexing.lexeme buf

let continue_comment buf =
  let comment = current_comment () in
  let c = Lexing.lexeme_char buf 0 in
  let ch = if c = '\r' then '\n' else c in
  comment.buffer <- comment.buffer ^ String.make 1 ch

let end_comment buf =
  let comment = current_comment () in
  comment.buffer <- comment.buffer ^ Lexing.lexeme buf

let trim s =
  let rec loop = function
    | 0 -> 0
    | i ->
	let c = s.[i-1] in
	if c = '\n' || c = '\r' then loop (i-1)
	else i
  in
  let len = String.length s in
  let n = loop len in
  if n < len then String.sub s 0 n
  else s

let eol_comment buf =
  let comment = new_comment () in
  comment.pos <- lexeme_pos buf;
  comment.buffer <- trim (Lexing.lexeme buf)

let with_lexbuf f =
  let chan = open_in !file in
  let cleanup () =
    close_in chan;
    Parsing.clear_parser ();
    comment_list := []
  in
  try
    let result = f (Lexing.from_channel chan) in
    cleanup ();
    result
  with e -> (cleanup (); raise e)
