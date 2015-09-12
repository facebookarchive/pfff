(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2015 Yoann Padioleau
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

module Ast = Ast_nw
open Highlight_code
module T = Lexer_nw
module TH = Token_helpers_nw

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: now that have a fuzzy AST for noweb, could use that
 * instead of those span_brace functions
 *)
let span_close_brace xs = xs +> Common2.split_when (function 
  | T.TCBrace _ -> true | _ -> false)

let span_newline xs = xs +> Common2.split_when (function 
  | T.TCommentNewline _ -> true | _ -> false)

let span_end_bracket xs = xs +> Common2.split_when (function 
  | T.TSymbol("]", _) -> true | _ -> false)

let tag_all_tok_with ~tag categ xs = 
  xs +> List.iter (fun tok ->
    let info = TH.info_of_tok tok in
    tag info categ
  )

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is.
 *)

let visit_program
    ~tag_hook
    _prefs 
    (*db_opt *)
    (_program, toks)
  =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)

  let rec aux_toks xs = 
    match xs with
    | [] -> ()
    (* a little bit pad specific *)
    |   T.TComment(ii)
      ::T.TCommentNewline (_ii2)
      ::T.TComment(ii3)
      ::T.TCommentNewline (_ii4)
      ::T.TComment(ii5)
      ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
        | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection0
        | _ when s =~ ".*------" && s5 =~ ".*------" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection1
        | _ when s =~ ".*####" && s5 =~ ".*####" ->
          tag ii CommentEstet;
          tag ii5 CommentEstet;
          tag ii3 CommentSection2
        | _ ->
            ()
        );
        aux_toks xs

    |    T.TCommand(("chapter" | "chapter*"),_)
      :: T.TOBrace _
      :: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag CommentSection0 before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    |    T.TCommand("section",_):: T.TOBrace _:: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag CommentSection1 before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    |    T.TCommand("subsection",_):: T.TOBrace _:: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag CommentSection2 before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    |    T.TCommand("subsubsection",_):: T.TOBrace _:: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag CommentSection3 before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    |    T.TCommand("label",_):: T.TOBrace _:: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag (Label Def) before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    |    T.TCommand("ref",_):: T.TOBrace _:: xs 
      ->
       let (before, _, _) = span_close_brace xs in
       tag_all_tok_with ~tag (Label Use) before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs


    (* noweb specific *)
    |  T.TSymbol("[", ii)::T.TSymbol("[", _)::xs ->
         let rest =
           try 
             let (before, middle, after) =
               span_end_bracket xs
             in
             tag_all_tok_with 
               ~tag:(fun ii categ -> 
                 if not (Hashtbl.mem already_tagged ii)
                 then tag ii categ
               )
               TypeVoid (* TODO *) before;
             (middle::after)
           with Not_found ->
             pr2 (spf "PB span_end_bracket at %d" 
                     (Parse_info.line_of_info ii));
             xs
         in
         aux_toks (rest);

    (* pad noweb specific *)
    | T.TSymbol("#", _ii)::T.TWord("include", ii2)::xs ->
        tag ii2 Include;
        aux_toks xs

    (* specific to texinfo *)
    |    T.TSymbol("@", _)
      :: T.TWord(s, ii)::xs ->
           let categ_opt = 
             (match s with
             | "title" -> Some CommentSection0
             | "chapter" -> Some CommentSection0
             | "section" -> Some CommentSection1
             | "subsection" -> Some CommentSection2
             | "subsubsection" -> Some CommentSection3
             | "c" -> Some Comment
             (* don't want to polluate my view with indexing "aspect" *)
             | "cindex" -> 
                 tag ii Comment;
                 Some Comment
             | _ -> None
             )
           in
           (match categ_opt with
           | None -> 
               tag ii Keyword;
               aux_toks xs
           | Some categ ->
               let (before, _, _) = span_newline xs in
               tag_all_tok_with ~tag categ before;
               (* repass on tokens, in case there are nested tex commands *)
               aux_toks xs
           )

    (* specific to web TeX source: ex: @* \[24] Getting the next token. *)
    |    T.TSymbol("@*", _)
      :: T.TCommentSpace _
      :: T.TSymbol("\\", _)
      :: T.TSymbol("[", ii1)
      :: T.TNumber(_, iinum)
      :: T.TSymbol("]", ii2)
      :: T.TCommentSpace _
      :: xs 
      ->
       let (before, _, _) = span_newline xs in
       [ii1;iinum;ii2] +> List.iter (fun ii -> tag ii CommentSection0);
       tag_all_tok_with ~tag CommentSection0 before;
       (* repass on tokens, in case there are nested tex commands *)
       aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  let toks' = toks +> Common.exclude (function
    (* needed ? *)
    (* | T.TCommentSpace _ -> true *)
    | _ -> false
  )
  in
  aux_toks toks';

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *) 
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)

  toks +> List.iter (fun tok -> 
    match tok with
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then 
         let s = Parse_info.str_of_info ii in
         (match s with
         | _ when s =~ "^%todo:" -> tag ii BadSmell
         | _ -> tag ii Comment
         )
    | T.TCommentSpace _ii -> ()
    | T.TCommentNewline _ii -> ()

    | T.TCommand (s, ii) -> 
        let categ = 
          (match s with
          | s when s =~ "^if" -> KeywordConditional
          | s when s =~ ".*true$" -> Boolean
          | s when s =~ ".*false$" -> Boolean
          | "fi" -> KeywordConditional
          | "input" -> Include
          | "appendix" -> CommentSection0
          | _ -> Keyword
          )
        in
        tag ii categ
      
    | T.TWord (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then
          ()

    (* noweb specific obviously *)
    | T.TBeginNowebChunk ii
    | T.TEndNowebChunk ii
      ->
        tag ii KeywordExn (* TODO *)

    | T.TNowebChunkLine (_, ii) | T.TNowebAngle ii ->
        tag ii EmbededCode

    | T.TBeginNowebChunkName ii
    | T.TEndNowebChunkName ii
      ->
        tag ii KeywordObject (* TODO *)

    | T.TNowebChunkName (ii)  ->
        tag ii KeywordObject (* TODO *)


    | T.TBeginVerbatim ii
    | T.TEndVerbatim ii
        -> 
        tag ii KeywordLoop

    | T.TVerbatimLine (_, ii) ->
        tag ii Verbatim
    (* very pad specific *)
    | T.TFootnote (c, ii) ->
        (match c with
        | 't' -> tag ii BadSmell
        | _ -> tag ii Comment
        )

    | T.TNumber (_, ii) -> 
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Number
    | T.TSymbol (_, ii) -> tag ii Punctuation

    | T.TOBrace ii | T.TCBrace ii ->  tag ii Punctuation

    | T.TUnknown ii -> tag ii Error
    | T.EOF _ii-> ()

  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)  

  ()
