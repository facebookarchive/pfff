(* Yoann Padioleau
 * 
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
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

module TH = Token_helpers_cpp

open Parser_cpp

module Parser = Parser_cpp

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_cpp.verbose_parsing 

(*****************************************************************************)
(* Fuzzy parsing, different "views" over the same program *)
(*****************************************************************************)

(* Normally I should not use ref/mutable in the token_extended type
 * and I should have a set of functions taking a list of tokens and
 * returning a list of tokens. The problem is that to make easier some
 * functions, it is better to work on better representation, on "views"
 * over this list of tokens. But then modifying those views and get
 * back from those views to the original simple list of tokens is
 * tedious. One way is to maintain next to the view a list of "actions"
 * (I was using a hash storing the charpos of the token and associating
 * the action) but it is tedious too. Simpler to use mutable/ref. We
 * use the same idea that we use when working on the Ast_c. *)

(* old: when I was using the list of "actions" next to the views, the hash
 * indexed by the charpos, there could have been some problems:
 * how my fake_pos interact with the way I tag and adjust token ?
 * because I base my tagging on the position of the token ! so sometimes
 * could tag another fakeInfo that should not be tagged ? 
 * fortunately I don't use anymore this technique.
 *)

(* update: quite close to the Place_c.Inxxx.
 * update: now that can have complex nesting of class/func in c++,
 *  this type is also present in duplicated form in lexer_parser2.ml
 * 
 * The strategy to set the tag is simply to look at the token 
 * before the '{'.
 *)
type context = 
  InFunction | InEnum | InStruct | InInitializer | NoContext

type token_extended = { 
  mutable tok: Parser.token;
  mutable where: context;

  (* less: need also a after ? *)
  mutable new_tokens_before : Parser.token list;

  (* line x col  cache, more easily accessible, of the info in the token *)
  line: int; 
  col : int;
}

let set_as_comment cppkind x = 
  (* normally the caller have first filtered the set of tokens to have
   * a clearer "view" to work on
   *)
  assert(not (TH.is_real_comment x.tok));

  if TH.is_eof x.tok 
  then () (* otherwise parse_c will be lost if don't find a EOF token *)
  else 
    x.tok <- TCommentCpp (cppkind, TH.info_of_tok x.tok)


let mk_token_extended x = 
  let (line, col) = TH.linecol_of_tok x in
  { tok = x; 
    line = line; col = col; 
    where = NoContext; 
    new_tokens_before = [];
  }

let rebuild_tokens_extented toks_ext = 
  let _tokens = ref [] in
  toks_ext +> List.iter (fun tok -> 
    tok.new_tokens_before +> List.iter (fun x -> push2 x _tokens);
    push2 tok.tok _tokens 
  );
  let tokens = List.rev !_tokens in
  (tokens +> acc_map mk_token_extended)


(* x list list, because x list separated by ',' *) 
type paren_grouped = 
  | Parenthised   of paren_grouped list list * token_extended list
  | PToken of token_extended

type brace_grouped = 
  | Braceised   of 
      brace_grouped list list * token_extended * token_extended option
  | BToken of token_extended

(* Far better data structure than doing hacks in the lexer or parser
 * because in lexer we don't know to which ifdef a endif is related
 * and so when we want to comment a ifdef, we don't know which endif
 * we must also comment. Especially true for the #if 0 which sometimes
 * have a #else part.
 * 
 * x list list, because x list separated by #else or #elif 
 *) 
type ifdef_grouped = 
  | Ifdef     of ifdef_grouped list list * token_extended list
  | Ifdefbool of bool * ifdef_grouped list list * token_extended list
  | NotIfdefLine of token_extended list


type 'a line_grouped = 
  Line of 'a list


type body_function_grouped = 
  | BodyFunction of token_extended list
  | NotBodyLine  of token_extended list

(* ------------------------------------------------------------------------- *)
(* view builders  *)
(* ------------------------------------------------------------------------- *)

(* todo: synchro ! use more indentation 
 * if paren not closed and same indentation level, certainly because
 * part of a mid-ifdef-expression.
*)
let rec mk_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x.tok with 
      | xx when TH.is_opar xx -> 
          let body, extras, xs = mk_parameters [x] [] xs in
          Parenthised (body,extras)::mk_parenthised xs
      | _ -> 
          PToken x::mk_parenthised xs
      )

(* return the body of the parenthised expression and the rest of the tokens *)
and mk_parameters extras acc_before_sep  xs = 
  match xs with
  | [] -> 
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
      pr2 "PB: not found closing paren in fuzzy parsing";
      [List.rev acc_before_sep], List.rev extras, []
  | x::xs -> 
      (match x.tok with 
      (* synchro *)
      | xx when TH.is_obrace xx && x.col = 0 -> 
          pr2 "PB: found synchro point } in paren";
          [List.rev acc_before_sep], List.rev (extras), (x::xs)

      | xx when TH.is_cpar xx  -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | xx when TH.is_opar xx -> 
          let body, extrasnest, xs = mk_parameters [x] [] xs in
          mk_parameters extras 
            (Parenthised (body,extrasnest)::acc_before_sep) 
            xs
      | TComma _ -> 
          let body, extras, xs = mk_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | _ -> 
          mk_parameters extras (PToken x::acc_before_sep) xs
      )




let rec mk_braceised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x.tok with 
      | xx when TH.is_obrace xx -> 
          let body, endbrace, xs = mk_braceised_aux [] xs in
          Braceised (body, x, endbrace)::mk_braceised xs
      | xx when TH.is_cbrace xx -> 
          pr2 "PB: found closing brace alone in fuzzy parsing";
          BToken x::mk_braceised xs
      | _ -> 
          BToken x::mk_braceised xs
      )

(* return the body of the parenthised expression and the rest of the tokens *)
and mk_braceised_aux acc xs = 
  match xs with
  | [] -> 
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
      pr2 "PB: not found closing brace in fuzzy parsing";
      [List.rev acc], None, []
  | x::xs -> 
      (match x.tok with 
      | xx when TH.is_cbrace xx -> [List.rev acc], Some x, xs
      | xx when TH.is_obrace xx -> 
          let body, endbrace, xs = mk_braceised_aux [] xs in
          mk_braceised_aux  (Braceised (body,x, endbrace)::acc) xs
      | _ -> 
          mk_braceised_aux (BToken x::acc) xs
      )

          


let rec mk_ifdef xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      (match x.tok with 
      | TIfdef _ -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdef (body, extra)::mk_ifdef xs
      | TIfdefBool (b,_) -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          
          (* if not passing, then consider a #if 0 as an ordinary #ifdef *)
          if !Flag.if0_passing
          then Ifdefbool (b, body, extra)::mk_ifdef xs
          else Ifdef(body, extra)::mk_ifdef xs

      | TIfdefMisc (b,_) | TIfdefVersion (b,_) -> 
          let body, extra, xs = mk_ifdef_parameters [x] [] xs in
          Ifdefbool (b, body, extra)::mk_ifdef xs

          
      | _ -> 
          (* todo? can have some Ifdef in the line ? *)
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
          NotIfdefLine line::mk_ifdef xs 
      )

and mk_ifdef_parameters extras acc_before_sep xs = 
  match xs with
  | [] -> 
      (* Note that mk_ifdef is assuming that CPP instruction are alone
       * on their line. Because I do a span (fun x -> is_same_line ...)
       * I might take with me a #endif if this one is mixed on a line
       * with some "normal" tokens.
       *)
      pr2 "PB: not found closing ifdef in fuzzy parsing";
      [List.rev acc_before_sep], List.rev extras, []
  | x::xs -> 
      (match x.tok with 
      | TEndif _ -> 
          [List.rev acc_before_sep], List.rev (x::extras), xs
      | TIfdef _ -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdef (body, extrasnest)::acc_before_sep) xs

      | TIfdefBool (b,_) -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in

          if !Flag.if0_passing
          then
            mk_ifdef_parameters 
              extras (Ifdefbool (b, body, extrasnest)::acc_before_sep) xs
          else 
            mk_ifdef_parameters 
              extras (Ifdef (body, extrasnest)::acc_before_sep) xs


      | TIfdefMisc (b,_) | TIfdefVersion (b,_) -> 
          let body, extrasnest, xs = mk_ifdef_parameters [x] [] xs in
          mk_ifdef_parameters 
            extras (Ifdefbool (b, body, extrasnest)::acc_before_sep) xs

      | TIfdefelse _ 
      | TIfdefelif _ -> 
          let body, extras, xs = mk_ifdef_parameters (x::extras) [] xs in
          (List.rev acc_before_sep)::body, extras, xs 
      | _ -> 
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
          mk_ifdef_parameters extras (NotIfdefLine line::acc_before_sep) xs
      )

(* --------------------------------------- *)

let line_of_paren = function
  | PToken x -> x.line
  | Parenthised (xxs, info_parens) -> 
      (match info_parens with
      | [] -> raise Impossible
      | x::xs -> x.line
      )

let rec span_line_paren line = function
  | [] -> [],[]
  | x::xs -> 
      (match x with
      | PToken tok when TH.is_eof tok.tok -> 
          [], x::xs
      | _ -> 
        if line_of_paren x = line 
        then
          let (l1, l2) = span_line_paren line xs in
          (x::l1, l2)
        else ([], x::xs)
      )

(* old        
let rec mk_line_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      let line_no = line_of_paren x in
      let line, xs = span_line_paren line_no xs in
      Line (x::line)::mk_line_parenthised xs
*)





(* --------------------------------------- *)

let line_range_of_paren = function
  | PToken x -> x.line, x.line
  | Parenthised (xxs, info_parens) -> 
      (match info_parens with
      | [] -> raise Impossible
      | x::xs -> 
          let lines_no = (x::xs) +> List.map (fun x -> x.line) in
          Common.minimum lines_no, Common.maximum lines_no
      )

let rec span_line_paren_range (imin, imax) = function
  | [] -> [],[]
  | x::xs -> 
      (match x with
      | PToken tok when TH.is_eof tok.tok -> 
          [], x::xs
      | _ -> 
        if line_of_paren x >= imin && line_of_paren x <= imax
        then
          (* may need to extend *)
          let (imin', imax') = line_range_of_paren x in
          let (l1, l2) = span_line_paren_range (imin, max imax imax') xs in
          (x::l1, l2)
        else ([], x::xs)
      )


let rec mk_line_parenthised xs = 
  match xs with
  | [] -> []
  | x::xs -> 
      let line_range = line_range_of_paren x in
      let line, xs = span_line_paren_range line_range xs in
      Line (x::line)::mk_line_parenthised xs





(* --------------------------------------- *)
let rec mk_body_function_grouped xs = 
  match xs with 
  | [] -> []
  | x::xs -> 
      (match x with
      | {tok = TOBrace _; col = 0; _} -> 
          let is_closing_brace = function 
            | {tok = TCBrace _; col = 0; _ } -> true 
            | _ -> false 
          in
          let body, xs = Common.span (fun x -> not (is_closing_brace x)) xs in
          (match xs with
          | ({tok = TCBrace _; col = 0; _ })::xs -> 
              BodyFunction body::mk_body_function_grouped xs
          | [] -> 
              pr2 "PB:not found closing brace in fuzzy parsing";
              [NotBodyLine body]
          | _ -> raise Impossible
          )
          
      | _ -> 
          let line, xs = Common.span (fun y -> y.line = x.line) (x::xs) in
          NotBodyLine line::mk_body_function_grouped xs 
      )

(* --------------------------------------- *)
let is_braceised = function
  | Braceised   _ -> true
  | BToken _ -> false

(* ------------------------------------------------------------------------- *)
(* view iterators  *)
(* ------------------------------------------------------------------------- *)

let rec iter_token_paren f xs = 
  xs +> List.iter (function
  | PToken tok -> f tok;
  | Parenthised (xxs, info_parens) -> 
      info_parens +> List.iter f;
      xxs +> List.iter (fun xs -> iter_token_paren f xs)
  )

let rec iter_token_brace f xs = 
  xs +> List.iter (function
  | BToken tok -> f tok;
  | Braceised (xxs, tok1, tok2opt) -> 
      f tok1; do_option f tok2opt;
      xxs +> List.iter (fun xs -> iter_token_brace f xs)
  )

let rec iter_token_ifdef f xs = 
  xs +> List.iter (function
  | NotIfdefLine xs -> xs +> List.iter f;
  | Ifdefbool (_, xxs, info_ifdef) 
  | Ifdef (xxs, info_ifdef) -> 
      info_ifdef +> List.iter f;
      xxs +> List.iter (iter_token_ifdef f)
  )




let tokens_of_paren xs = 
  let g = ref [] in
  xs +> iter_token_paren (fun tok -> push2 tok g);
  List.rev !g


let tokens_of_paren_ordered xs = 
  let g = ref [] in

  let rec aux_tokens_ordered = function
    | PToken tok -> push2 tok g;
    | Parenthised (xxs, info_parens) -> 
        let (opar, cpar, commas) = 
          match info_parens with
          | opar::xs -> 
              (match List.rev xs with
              | cpar::xs -> 
                  opar, cpar, List.rev xs
              | _ -> raise Impossible
              )
          | _ -> raise Impossible
        in
        push2 opar g;
        aux_args (xxs,commas);
        push2 cpar g;

  and aux_args (xxs, commas) =
    match xxs, commas with
    | [], [] -> ()
    | [xs], [] -> xs +> List.iter aux_tokens_ordered
    | xs::ys::xxs, comma::commas -> 
        xs +> List.iter aux_tokens_ordered;
        push2 comma g;
        aux_args (ys::xxs, commas)
    | _ -> raise Impossible

  in

  xs +> List.iter aux_tokens_ordered;
  List.rev !g



(* ------------------------------------------------------------------------- *)
(* set the context info in token *)
(* ------------------------------------------------------------------------- *)
(* All important context are introduced via some '{' '}'. To make the 
 * difference is it often enough to just look at the few tokens before the
 * '{'.
 * 
 * TODO harder now that have c++, can have function inside struct so need
 * handle all together. 
 * 
 * TODO So change token but do not recurse in
 * nested Braceised. maybe do via accumulator, don't use iter_token_brace.
 * 
 * TODO Also need remove the qualifier as they make
 * the sequence pattern matching more difficult.
 *)

let rec set_in_function_tag xs = 
 (* could try: ) { } but it can be the ) of a if or while, so 
  * better to base the heuristic on the position in column zero.
  * Note that some struct or enum or init put also their { in first column
  * but set_in_other will overwrite the previous InFunction tag.
  *)
 let rec aux xs = 
  match xs with
  | [] -> ()
(*TODOC++ext: now can have some const or throw between 
  => do a view that filter them first ?
*)

  (* ) { and the closing } is in column zero, then certainly a function *)
(*TODO1 col 0 not valid anymore with c++ nestedness of method *)
  | BToken ({tok = TCPar _;_})::(Braceised (body, tok1, Some tok2))::xs 
      when tok1.col <> 0 && tok2.col = 0 -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InFunction
      ));
      aux xs

  | (BToken x)::xs -> aux xs

(*TODO1 not valid anymore with c++ nestedness of method *)
  | (Braceised (body, tok1, Some tok2))::xs 
      when tok1.col = 0 && tok2.col = 0 -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InFunction
      ));
      aux xs
  | Braceised (body, tok1, tok2)::xs -> 
      aux xs
 in
 aux xs



  

let rec set_in_other xs = 
  match xs with 
  | [] -> ()
  (* enum x { } *)
  | BToken ({tok = Tenum _;_})::BToken ({tok = TIdent _;_})
    ::Braceised(body, tok1, tok2)::xs 
  | BToken ({tok = Tenum _;_})
    ::Braceised(body, tok1, tok2)::xs 
    -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InEnum;
      ));
      set_in_other xs
  (* struct/union/class x { } *)
  | BToken ({tok = tokstruct; _})::BToken ({tok = TIdent _; _})
    ::Braceised(body, tok1, tok2)::xs when TH.is_classkey_keyword tokstruct -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InStruct;
      ));
      set_in_other xs

  (* struct/union/class x : ... { } *)
  | BToken ({tok = tokstruct; _})::BToken ({tok = TIdent _; _})
    ::BToken ({tok = TCol _;_})::xs when TH.is_classkey_keyword tokstruct -> 
      
      let (before, elem, after) = Common.split_when is_braceised xs in
      (match elem with 
      | Braceised(body, tok1, tok2) -> 
          body +> List.iter (iter_token_brace (fun tok -> 
            tok.where <- InInitializer;
          ));
          set_in_other after
      | _ -> raise Impossible
      )
          

  (* = { } *)
  | BToken ({tok = TEq _; _})
    ::Braceised(body, tok1, tok2)::xs -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InInitializer;
      ));
      set_in_other xs


  (* recurse *)
  | BToken _::xs -> set_in_other xs
  | Braceised(body, tok1, tok2)::xs -> 
      body +> List.iter set_in_other;
      set_in_other xs

      
      

let set_context_tag xs = 
  begin
    (* order is important *)
    set_in_function_tag xs;
    set_in_other xs;
  end
