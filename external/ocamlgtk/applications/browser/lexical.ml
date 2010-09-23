(**************************************************************************)
(*     Lablgtk - Applications                                             *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*      Benjamin Monate  <Benjamin.Monate@free.fr>                        *)
(*      Olivier Andrieu  <oandrieu@nerim.net>                             *)
(*      Jun Furuse       <Jun.Furuse@inria.fr>                            *)
(*      Hubert Fauque    <hubert.fauque@wanadoo.fr>                       *)
(*      Koji Kagawa      <kagawa@eng.kagawa-u.ac.jp>                      *)
(*                                                                        *)
(**************************************************************************)

(* $Id: lexical.ml 1352 2007-07-12 08:56:18Z zoggy $ *)

open StdLabels
open Parser
open Lexing

let tags =
  ["control"; "define"; "structure"; "char";
   "infix"; "label"; "uident"]
and colors =
    ["blue"; "forestgreen"; "purple"; "gray40";
     "indianred4"; "saddlebrown"; "midnightblue"]

let init_tags (tb : GText.buffer) =
  List.iter2 tags colors ~f:
  begin fun tag col ->
    ignore (tb#create_tag ~name:tag [`FOREGROUND col])
  end;
  tb#create_tag ~name:"error" [`FOREGROUND "red"; `WEIGHT `BOLD];
  ()

let tpos ~(start : GText.iter) pos =
  let l = pos.pos_lnum - 1 in
  if l = 0 then
    start#set_line_index (pos.pos_cnum + start#line_index)
  else
    (start#forward_lines l)#set_line_index (pos.pos_cnum - pos.pos_bol)

let tag ?start ?stop (tb : GText.buffer) =
  let start = Gaux.default tb#start_iter ~opt:start
  and stop = Gaux.default tb#end_iter ~opt:stop in
  (* Printf.printf "tagging: %d-%d\n" start#offset stop#offset;
     flush stdout; *)
  let tpos = tpos ~start in
  let text = tb#get_text ~start ~stop () in
  let buffer = Lexing.from_string text in
  tb#remove_all_tags ~start ~stop;
  let last = ref (EOF, dummy_pos, dummy_pos) in
  try
    while true do
    let token = Lexer.token buffer
    and start = Lexing.lexeme_start_p buffer
    and stop = Lexing.lexeme_end_p buffer in
    let tag =
      match token with
        AMPERAMPER
      | AMPERSAND
      | BARBAR
      | DO | DONE
      | DOWNTO
      | ELSE
      | FOR
      | IF
      | LAZY
      | MATCH
      | OR
      | THEN
      | TO
      | TRY
      | WHEN
      | WHILE
      | WITH
          -> "control"
      | AND
      | AS
      | BAR
      | CLASS
      | CONSTRAINT
      | EXCEPTION
      | EXTERNAL
      | FUN
      | FUNCTION
      | FUNCTOR
      | IN
      | INHERIT
      | INITIALIZER
      | LET
      | METHOD
      | MODULE
      | MUTABLE
      | NEW
      | OF
      | PRIVATE
      | REC
      | TYPE
      | VAL
      | VIRTUAL
          -> "define"
      | BEGIN
      | END
      | INCLUDE
      | OBJECT
      | OPEN
      | SIG
      | STRUCT
          -> "structure"
      | CHAR _
      | STRING _
          -> "char"
      | BACKQUOTE
      | INFIXOP1 _
      | INFIXOP2 _
      | INFIXOP3 _
      | INFIXOP4 _
      | PREFIXOP _
      | SHARP
          -> "infix"
      | LABEL _
      | OPTLABEL _
      | QUESTION
      | TILDE
          -> "label"
      | UIDENT _ -> "uident"
      | LIDENT _ ->
          begin match !last with
            (QUESTION | TILDE), _, _ -> "label"
          | _ -> ""
          end
      | COLON ->
          begin match !last with
            LIDENT _, lstart, lstop ->
              if lstop.pos_cnum = start.pos_cnum then
                tb#apply_tag_by_name "label"
                  ~start:(tpos lstart) ~stop:(tpos stop);
              ""
          | _ -> ""
          end
      | EOF -> raise End_of_file
      | _ -> ""
    in
    if tag <> "" then begin
      (* Printf.printf "%d-%d: %s\n" start.pos_cnum stop.pos_cnum tag;
         flush stdout; *)
      tb#apply_tag_by_name tag ~start:(tpos start) ~stop:(tpos stop);
    end;
    last := (token, start, stop)
    done
  with
    End_of_file -> ()
  | Lexer.Error _ -> ()
