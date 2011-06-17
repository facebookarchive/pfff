(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

open Parser_cpp

module Ast = Ast_cpp
module Parser = Parser_cpp
module TH = Token_helpers_cpp
module Hack = Parsing_hacks_lib

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)

(* 
 * To parse macro definitions I need to do some tricks 
 * as some information can be computed only at the lexing level. For instance
 * the space after the name of the macro in '#define foo (x)' is meaningful
 * but the grammar does not have this information. So define_ident() below
 * look at such space and generate a special TOpar_Define token. 
 * 
 * In a similar way macro definitions can contain some antislash and newlines
 * and the grammar need to know where the macro ends which is
 * a line-level and so low token-level information. Hence the 
 * function define_line'()below and the TCommentNewline_DefineEndOfMacro.
 * 
 * update: TCommentNewline_DefineEndOfMacro is handled in a special way
 * at different places, a little bit like EOF, especially for error recovery,
 * so this is an important token that should not be retagged!
 * 
 * We also change the kind of TIdent to TIdent_Define to avoid bad interactions
 * with other parsing_hack tricks. For instant if keep TIdent then
 * the stringication heuristics can believe the TIdent is a string-macro.
 * So simpler to change the kind of the TIdent in a macro too.
 *
 * ugly hack, a better solution perhaps would be to erase
 * TCommentNewline_DefineEndOfMacro from the Ast and list of tokens in parse_c.
 * 
 * note: I do a +1 somewhere, it's for the unparsing to correctly sync.
 * 
 * note: can't replace mark_end_define by simply a fakeInfo(). The reason
 * is where is the \n TCommentSpace. Normally there is always a last token
 * to synchronize on, either EOF or the token of the next toplevel.
 * In the case of the #define we got in list of token 
 * [TCommentSpace "\n"; TDefEOL] but if TDefEOL is a fakeinfo then we will
 * not synchronize on it and so we will not print the "\n".
 * A solution would be to put the TDefEOL before the "\n".
 * 
 * todo?: could put a ExpandedTok for that ? 
 *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let mark_end_define ii = 
  let ii' = 
    { Parse_info.
      token = Parse_info.OriginTok { 
        (Ast.parse_info_of_info ii) with 
          Parse_info.str = ""; 
          Parse_info.charpos = Ast.pos_of_info ii + 1
      };
      transfo = Parse_info.NoTransfo;
      comments = ();
    } 
  in
  (* fresh_tok *) TCommentNewline_DefineEndOfMacro (ii')

(* Sometimes I prefer to generate a single token for a list of things in the
 * lexer so that if I have to passed them, liking passing TInclude then
 * it's easy. Also if I don't do a single token, then I need to 
 * parse the rest which may not need special stuff, like detecting 
 * end of line which the parser is not really ready for. So for instance
 * could I parse a #include <a/b/c/xxx.h> as 2 or more tokens ? just
 * lex #include ? so then need recognize <a/b/c/xxx.h> as one token ? 
 * but this kind of token is valid only after a #include and the
 * lexing and parsing rules are different for such tokens so not that
 * easy to parse such things in parser_c.mly. Hence the following hacks.
 * 
 * less?: maybe could get rid of this like I get rid of some of fix_define.
 *)
(* used to generate new token from existing one *)
let new_info posadd str ii =
  { Parse_info.token = 
      Parse_info.OriginTok { (Parse_info.parse_info_of_info ii) with 
        Parse_info.
        charpos = Parse_info.pos_of_info ii + posadd;
        str     = str;
        column = Parse_info.col_of_info ii + posadd;
      };
    comments = ();
    transfo = Parse_info.NoTransfo;
   }

(* exported helpers *)
let rec comment_until_defeol xs = 
  match xs with
      
  | [] -> 
      (* job not done in Cpp_token_c.define_parse ? *)
      failwith "cant find end of define token TDefEOL"
  | x::xs -> 
      (match x with
      | Parser.TCommentNewline_DefineEndOfMacro i -> 
          (* fresh_tok *) 
            Parser.TComment_Cpp (Token_cpp.CppDirective, TH.info_of_tok x)
          ::xs
      | _ -> 
          let x' = 
            (* bugfix: otherwise may lose a TComment token *)
            if TH.is_real_comment x
            then x
            else
            (* fresh_tok *) 
              Parser.TComment_Cpp (Token_cpp.CppOther, TH.info_of_tok x)
          in
          x'::comment_until_defeol xs
      )

let drop_until_defeol xs = 
  xs +> Common.drop_until (function 
    Parser.TCommentNewline_DefineEndOfMacro _ -> true | _ -> false)
  +> List.tl

(*****************************************************************************)
(* Parsing hacks for #define *)
(*****************************************************************************)

(* put the TCommentNewline_DefineEndOfMacro at the good place *)
let rec define_line_1 xs = 
  match xs with
  | [] -> []
  | TDefine ii::xs -> 
      let line = Ast.line_of_info ii in
      TDefine ii::define_line_2 line ii xs
  | TCppEscapedNewline ii::xs -> 
      pr2 "WIERD: a \\ outside a #define";
      (* fresh_tok*) TCommentSpace ii::define_line_1 xs
  | x::xs -> 
      x::define_line_1 xs

and define_line_2 line lastinfo xs = 
  match xs with 
  | [] -> 
      (* should not happened, should meet EOF before *)
      pr2 "PB: WIERD";   
      mark_end_define lastinfo::[]
  | x::xs -> 
      let line' = TH.line_of_tok x in
      let info = TH.info_of_tok x in

      (match x with
      | EOF ii -> 
          mark_end_define lastinfo::EOF ii::define_line_1 xs
      | TCppEscapedNewline ii -> 
          if (line' <> line) 
          then pr2 "PB: WIERD: not same line number";
          (* fresh_tok*) TCommentSpace ii::define_line_2 (line+1) info xs
      | x -> 
          if line' = line
          then x::define_line_2 line info xs 
          else 
            mark_end_define lastinfo::define_line_1 (x::xs)
      )

let rec define_ident xs = 
  match xs with
  | [] -> []
  | TDefine ii::xs -> 
      TDefine ii::
      (match xs with
      | TCommentSpace i1::TIdent (s,i2)::(* no space *)TOPar (i3)::xs -> 
          (* if TOPar_Define is just next to the ident (no space), then
           * it's a macro-function. We change the token to avoid
           * ambiguity between '#define foo(x)'  and   '#define foo   (x)'
           *)
            TCommentSpace i1
          ::Hack.fresh_tok (TIdent_Define (s,i2))
          ::Hack.fresh_tok (TOPar_Define i3)
          ::define_ident xs

      | TCommentSpace i1::TIdent (s,i2)::xs -> 
            TCommentSpace i1
          ::Hack.fresh_tok (TIdent_Define (s,i2))
          ::define_ident xs
      | _ -> 
          pr2 "wierd #define body"; 
          define_ident xs
      )
  | x::xs -> 
      x::define_ident xs

let fix_tokens_define2 xs = 
  define_ident (define_line_1 xs)

let fix_tokens_define a = 
  Common.profile_code "Hack.fix_define" (fun () -> fix_tokens_define2 a)

(*****************************************************************************)
(* Parsing hacks for #include *)
(*****************************************************************************)
(* returns a pair (replaced token, list of next tokens) *)

(* todo: move in a fix_tokens style *)
let tokens_include (info, includes, filename, inifdef) = 
  (* fresh_tok *) 
   Parser.TInclude_Start (Parse_info.rewrap_str includes info, inifdef),
  [ (* fresh_tok *) Parser.TInclude_Filename 
      (filename, (new_info (String.length includes) filename info))
  ]
