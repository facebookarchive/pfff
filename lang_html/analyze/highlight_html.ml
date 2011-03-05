(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

open Ast_html

module Ast = Ast_html
module V = Visitor_html

open Highlight_code

module T = Parser_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)
let visit_toplevel ~tag_hook prefs (toplevel, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 
  (* -------------------------------------------------------------------- *)

  (* if href, then EmbededUrl *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  (* 
   * note: all TCommentSpace are filtered in xs so easier to write
   * rules (but regular comments are kept as well as newlines).
   *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)
  toks +> List.iter (fun tok -> 
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          tag ii Comment

    | T.Space ii -> ()
    | T.EOF ii -> ()

    | T.Eq ii -> tag ii Punctuation

    | T.Lelement (ii, s) ->
        (* todo: different color depending on element ? *)
        tag ii Keyword

    | T.Lelementend (ii, s) ->
        (* todo: better category *)
        tag ii (Module Use);

    | T.Name (ii, s) ->
        (* todo: different color depending on attr ? *)
        tag ii TypeMisc

    | T.Relement ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Keyword
    | T.Relement_empty ii ->
        ()

    | T.Literal (ii, s) -> 
        if not (Hashtbl.mem already_tagged ii)
        then tag ii String

    | T.Other (ii) -> tag ii NotParsed

    | T.Cdata (ii, s) ->
        (* tag ii String ? *)
        ()

    | T.TPi (ii)
    | T.TDoctype (ii)
      ->
        ()
  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  
  (* -------------------------------------------------------------------- *)

  ()
