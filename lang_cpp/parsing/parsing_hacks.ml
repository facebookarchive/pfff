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

module Flag = Flag_parsing_cpp
module Ast = Ast_cpp

module TH = Token_helpers_cpp
module LP = Lexer_parser_cpp
module Parser = Parser_cpp

open Parser_cpp
open Token_views_cpp

open Parsing_hacks_lib
open Parsing_hacks_pp
open Parsing_hacks_cpp

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing hacks for define  *)
(*****************************************************************************)

(* To parse macro definitions I need to do some tricks 
 * as some information can be get only at the lexing level. For instance
 * the space after the name of the macro in '#define foo (x)' is meaningful
 * but the grammar can not get this information. So define_ident below
 * look at such space and generate a special TOpardefine. In a similar
 * way macro definitions can contain some antislash and newlines
 * and the grammar need to know where the macro ends (which is 
 * a line-level and so low token-level information). Hence the 
 * function 'define_line' below and the TDefEol.
 * 
 * update: TDefEol is handled in a special way at different places, 
 * a little bit like EOF, especially for error recovery, so this
 * is an important token that should not be retagged!
 * 
 * 
 * ugly hack, a better solution perhaps would be to erase TDefEOL 
 * from the Ast and list of tokens in parse_c. 
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
  TDefEOL (ii')

(* put the TDefEOL at the good place *)
let rec define_line_1 xs = 
  match xs with
  | [] -> []
  | TDefine ii::xs -> 
      let line = Ast.line_of_info ii in
      TDefine ii::define_line_2 line ii xs
  | TCppEscapedNewline ii::xs -> 
      pr2 "WIERD: a \\ outside a #define";
      TCommentSpace ii::define_line_1 xs
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
          if (line' <> line) then pr2 "PB: WIERD: not same line number";
          TCommentSpace ii::define_line_2 (line+1) info xs
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
      | TCommentSpace i1::TIdent (s,i2)::TOPar (i3)::xs -> 
          (* Change also the kind of TIdent to avoid bad interaction
           * with other parsing_hack tricks. For instant if keep TIdent then
           * the stringication algo can believe the TIdent is a string-macro.
           * So simpler to change the kind of the ident too.
           *)
          (* if TOParDefine sticked to the ident, then 
           * it's a macro-function. Change token to avoid ambiguity
           * between #define foo(x)  and   #define foo   (x)
           *)
          TCommentSpace i1::TIdentDefine (s,i2)::TOParDefine i3
          ::define_ident xs
      | TCommentSpace i1::TIdent (s,i2)::xs -> 
          TCommentSpace i1::TIdentDefine (s,i2)::define_ident xs
      | _ -> 
          pr2 "wierd #define body"; 
          define_ident xs
      )
  | x::xs -> 
      x::define_ident xs

let fix_tokens_define2 xs = 
  define_ident (define_line_1 xs)

let fix_tokens_define a = 
  Common.profile_code "C parsing.fix_define" (fun () -> fix_tokens_define2 a)

(* ------------------------------------------------------------------------- *)
(* Other parsing hacks related to cpp, Include/Define hacks *)
(* ------------------------------------------------------------------------- *)

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

(* helpers *)
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

let rec comment_until_defeol xs = 
  match xs with
      
  | [] -> 
      (* job not done in Cpp_token_c.define_parse ? *)
      failwith "cant find end of define token TDefEOL"
  | x::xs -> 
      (match x with
      | Parser.TDefEOL i -> 
          Parser.TCommentCpp (Token_cpp.CppDirective, TH.info_of_tok x)
          ::xs
      | _ -> 
          let x' = 
            (* bugfix: otherwise may lose a TComment token *)
            if TH.is_real_comment x
            then x
            else Parser.TCommentCpp (Token_cpp.CppOther, TH.info_of_tok x)
          in
          x'::comment_until_defeol xs
      )

let drop_until_defeol xs = 
  List.tl 
    (Common.drop_until (function Parser.TDefEOL _ -> true | _ -> false) xs)

(* ------------------------------------------------------------------------- *)
(* returns a pair (replaced token, list of next tokens) *)
(* ------------------------------------------------------------------------- *)

let tokens_include (info, includes, filename, inifdef) = 
  Parser.TIncludeStart (Parse_info.rewrap_str includes info, inifdef), 
  [Parser.TIncludeFilename 
      (filename, (new_info (String.length includes) filename info))
  ]


(* ------------------------------------------------------------------------- *)
(* main fix cpp function *)
(* ------------------------------------------------------------------------- *)

let filter_cpp_stuff xs = 
  let rec aux xs = 
    match xs with
    | [] -> []
    | x::xs -> 
        (match x.tok with
        | tok when TH.is_comment tok -> aux xs
        (* don't want drop the define, or if drop, have to drop
         * also its body otherwise the line heuristics may be lost
         * by not finding the TDefine in column 0 but by finding
         * a TDefineIdent in a column > 0
         *)
        | Parser.TDefine _ -> 
            x::aux xs
        | tok when TH.is_pp_instruction tok -> aux xs
        | _ -> x::aux xs
        )
  in
  aux xs
          

let insert_virtual_positions l =
  let strlen x = String.length (Ast.str_of_info x) in
  let rec loop prev offset = function
      [] -> []
    | x::xs ->
	let ii = TH.info_of_tok x in
	let inject pi =
	  TH.visitor_info_of_tok (function ii -> Ast.rewrap_pinfo pi ii) x in
	match Ast.pinfo_of_info ii with
	  Parse_info.OriginTok pi ->
	    let prev = Ast.parse_info_of_info ii in
	    x::(loop prev (strlen ii) xs)
	| Parse_info.ExpandedTok (pi,_, _) ->
	    inject (Parse_info.ExpandedTok (pi, prev,offset)) ::
	    (loop prev (offset + (strlen ii)) xs)
	| Parse_info.FakeTokStr (s,_) ->
	    inject (Parse_info.FakeTokStr (s, (Some (prev,offset)))) ::
	    (loop prev (offset + (strlen ii)) xs)
	| Parse_info.Ab -> failwith "abstract not expected" in
  let rec skip_fake = function
      [] -> []
    | x::xs ->
	let ii = TH.info_of_tok x in
	match Ast.pinfo_of_info ii with
	  Parse_info.OriginTok pi ->
	    let prev = Ast.parse_info_of_info ii in
	    x::(loop prev (strlen ii) xs)
	| _ -> x::skip_fake xs in
  skip_fake l


(*****************************************************************************)
(* Fix tokens_cpp *)
(*****************************************************************************)

let fix_tokens_cpp2 ~macro_defs tokens = 
  let tokens2 = ref (tokens +> acc_map mk_token_extended) in
  
  begin 
    (* the order is important, if you put the action heuristic first,
     * then because of ifdef, can have not closed paren
     * and so may believe that higher order macro 
     * and it will eat too much tokens. So important to do 
     * first the ifdef.
     * 
     * I recompute multiple times cleaner cos the mutable
     * can have be changed and so may have more comments
     * in the token original list.
     * 
     *)

    (* before filtering space and comments, want closed tokens *)
    find_cplusplus_view_all_tokens !tokens2;

    (* ifdef *)
    let cleaner = !tokens2 +> List.filter (fun x -> 
      not (TH.is_comment x.tok) (* could filter also #define/#include *)
    ) in
    let ifdef_grouped = mk_ifdef cleaner in
    find_ifdef_funheaders ifdef_grouped;
    find_ifdef_bool       ifdef_grouped;
    find_ifdef_mid        ifdef_grouped;


    (* macro 1 *)
    let cleaner = !tokens2 +> filter_cpp_stuff in

    let paren_grouped = mk_parenthised  cleaner in
    Pp_token.apply_macro_defs 
      macro_defs
      paren_grouped;
    (* because the before field is used by apply_macro_defs *)
    tokens2 := rebuild_tokens_extented !tokens2; 

    (* tagging contextual info (InFunc, InStruct, etc). Better to do
     * that after the "ifdef-simplification" phase.
     *)
    let cleaner = !tokens2 +> List.filter (fun x -> 
      not (TH.is_comment x.tok) (* could filter also #define/#include *)
    ) in

    (* done on brace_grouped but actually modifies tokens2 *)
    let brace_grouped = mk_braceised cleaner in
    set_context_tag   brace_grouped;



    (* macro *)
    let cleaner = !tokens2 +> filter_cpp_stuff in

    let paren_grouped      = mk_parenthised  cleaner in
    let line_paren_grouped = mk_line_parenthised paren_grouped in
    find_define_init_brace_paren paren_grouped;
    find_string_macro_paren paren_grouped;
    find_macro_lineparen    line_paren_grouped;
    find_macro_paren        paren_grouped;

    find_cplusplus_view_filtered_tokens cleaner;
    find_cplusplus_view_parenthized paren_grouped;
    find_cplusplus_view_parenthized2 paren_grouped;
    find_cplusplus_view_line_paren line_paren_grouped;
    find_cplusplus_view_filtered_tokens_bis cleaner;

    (* actions *)
    let cleaner = !tokens2 +> filter_cpp_stuff in
    let paren_grouped = mk_parenthised  cleaner in
    find_actions  paren_grouped;


    insert_virtual_positions (!tokens2 +> acc_map (fun x -> x.tok))
  end

let fix_tokens_cpp ~macro_defs a = 
  Common.profile_code "C parsing.fix_cpp" (fun () -> 
    fix_tokens_cpp2 ~macro_defs a)
      
(*****************************************************************************)
(* Lexing with lookahead *)
(*****************************************************************************)

(* Why using yet another parsing_hack technique ? The fix_xxx where do
 * some pre-processing on the full list of tokens is not enough ? 
 * No cos sometimes we need more contextual info, and even if
 * set_context() tries to give some contextual info, it's not completely
 * accurate so the following code give yet another alternative, yet another
 * chance to transform some tokens.
 * 
 * todo?: maybe could try to get rid of this technique. Maybe a better
 * set_context() would make possible to move this code using a fix_xx
 * technique.
 * 
 * This function work on a "cleaned" set of tokens, no space, no comment, 
 * no cpp.
 * c++ext: also no classname::
 *
 * 
 * LALR(k) trick. We can do stuff by adding cases in lexer_c.mll, but
 * it is more general to do it via my LALR(k) tech. Because here we can
 * transform some token give some context information. So sometimes it
 * makes sense to transform a token in one context, sometimes not, and
 * lex can not provide us this context information. Note that the order
 * in the pattern matching in lookahead is important. Do not cut/paste. 
 * 
 * Note that in next there is only "clean" tokens, there is no comment
 * or space tokens. This is done by the caller.
 * 
 * 
 * c++ext: 
 *  - add a |TAnd _ where had a TMul _
 *)

open Lexer_parser_cpp (* for the fields of lexer_hint type *)

let not_struct_enum = function
  | x::xs when TH.is_struct_like_keyword x -> false
  | [] -> true
  | x::xs -> true



let lookahead2 next before = 
 (* TODO *)
 let pass = 2 in

  match (next, before) with

  (* c++ext: constructed objects part2 *)
  (* > xx(   and in function *)
  | TOPar i1::_,           TIdent(s,i2)::TSup2 _::_ 
      when (LP.current_context () = (LP.InFunction)) -> 
        pr2_cplusplus("constructed_object: "  ^s);
        TOParCplusplusInit i1    

  (* yy xx(   and in function *)
  | TOPar i1::_,              TIdent(s,i2)::TypedefIdent _::_ 
      when (LP.current_context () = (LP.InFunction)) -> 
        pr2_cplusplus("constructed_object: "  ^s);
        TOParCplusplusInit i1    

  (* int xx( and in function, problably not a internal declaration *)
  | TOPar i1::_,   TIdent(s,i2)::t1::_ 
      when (LP.current_context () = (LP.InFunction)) &&
        TH.is_basic_type t1
        -> 
        pr2_cplusplus("constructed_object: "  ^s);
        TOParCplusplusInit i1    
        


  (* c++ext: for cast_function_expression and constructed expression.
   * Can only match cast_function_expression ? can also match destructor ...
   * 
   * note: the order is important
   * 
   * false positif of cast_function_typedef
   *  
   * typedef xxx ( *yyy) ...
   * new xxx(...)
   * new (placement_opt) xxx(...)
   *)
  | (TypedefIdent(s,i1))::TOPar _::_ , ((Ttypedef _|Tnew _ |TCPar _)::_) 
      ->
      TypedefIdent(s,i1)

  | tok::TOPar _::_ , ((Ttypedef _|Tnew _ |TCPar _)::_) 
     when TH.is_basic_type tok  ->
      tok

  (* addon: inline XXX() *)
  | ((TIdent(s,i1)|TypedefIdent(s,i1))::TOPar _::_, 
    (((TOBrace _| TCBrace _|TPtVirg _ | Texplicit _  |Tinline _)::_)
     (* for    public:   xx * y; *)
      | TCol _::(Tpublic _ |Tprotected _|Tprivate _)::_
    )) when (LP.current_context () = (LP.InClassStruct s))
        -> 
      msg_constructorname s;
      Tconstructorname(s,i1)


  | (TypedefIdent(s,i1))::TOPar _::_ , _ 
    -> 
      pr2_cplusplus("cast_function_typedef: "  ^s);
      TypedefIdent2(s,i1)


  (* basic type can now also be used as "cast_function/constructor" *)
  | (Tchar(i1))::TOPar _::_ , _  
      when (LP.current_context() <> LP.InParameter)
        ->
      pr2_cplusplus("cast_function_typedef: "  ^"char");
      Tchar2(i1)
  | (Tint(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"int");
      Tint2(i1)
  | (Tfloat(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"float");
      Tfloat2(i1)
  | (Tdouble(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"double");
      Tdouble2(i1)
  | (Tshort(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"short");
      Tshort2(i1)
  | (Tlong(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"long");
      Tlong2(i1)

  | (Tbool(i1))::TOPar _::_ , _   
      when (LP.current_context() <> LP.InParameter)
        -> 
      pr2_cplusplus("cast_function_typedef: "  ^"bool");
      Tbool2(i1)




  (* new foo() *)
  | TIdent(s,i1)::_, Tnew _::_ -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* template<xxx  on xxx *)
  | TIdent(s,i1)::_,      TInf2 _::_ -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* template<xxx, yyy  on yyy *)
  | TIdent(s,i1)::_,      TComma _::TypedefIdent(_)::TInf2 _::_ -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

      

  (*-------------------------------------------------------------*)
  (* typedef inference, parse_typedef_fix3 *)
  (*-------------------------------------------------------------*)
  (* xx xx *)
  | (TIdent(s,i1)::TIdent(s2,i2)::_ , _) when not_struct_enum before && s = s2
      && ok_typedef s
      (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
    -> 
      (* parse_typedef_fix3:
       *    acpi_object		acpi_object;
       * etait mal pars'e, car pas le temps d'appeler dt()  dans le type_spec. 
       * Le parser en interne a deja appel'e le prochain token pour pouvoir
       * decider des choses.
       *  => special case in lexer_heuristic, again
       *)
      if !Flag.debug_typedef 
      then pr2 ("TYPEDEF: disable typedef cos special case: " ^ s); 

      LP.disable_typedef();

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx yy *)
  | (TIdent (s, i1)::TIdent (s2, i2)::_  , _) 
      when not_struct_enum before 
      && ok_typedef s
        ->
         (* && not_annot s2 BUT lead to false positive*)

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx inline *)
  | (TIdent (s, i1)::Tinline i2::_  , _) when not_struct_enum before 
      && ok_typedef s
      -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* [,(] xx [,)] AND param decl *)
  | (TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s
      -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* [,(] xx =  AND param decl  for c++ext:  *)
  | (TIdent (s, i1)::TEq _ ::_ , (TComma _|TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s
      -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)
  (* xx* [,)]    '*' or '&' *)
  (* specialcase:  [,(] xx* [,)] *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
    -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)
  (* xx** [,)] *)
  (* specialcase:  [,(] xx** [,)] *)
  | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
    -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* specialcase:  xx*& [,)] *)
  | (TIdent (s, i1)::TMul _::TAnd _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
    -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)



  (* just look for const xx ? no!! cos can have char const xx; *)

  (* const xx *)
  | (TIdent (s, i1))::xs,  Tconst _::_ 
     when (LP.current_context() = LP.InTemplateParam) 
       && ok_typedef s  ->

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx const *   USELESS because of next rule ? *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::TMul _::_ , _ ) 
      when not_struct_enum before 
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s
      ->

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)
  
  (* xx const *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::_ , _ ) 
      when not_struct_enum before 
      && ok_typedef s
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      ->

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx * const *)
  | (TIdent (s, i1)::TMul _::(Tconst _ | Tvolatile _|Trestrict _)::_ , _ ) 
      when not_struct_enum before 
      && ok_typedef s
      ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)

  (* ( const xx)  *)
  | (TIdent (s, i1)::TCPar _::_,  (Tconst _ | Tvolatile _|Trestrict _)::TOPar _::_) when
      ok_typedef s ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)
      


  (* ( xx ) [sizeof, ~]  c++ext: TString *)
  | (TIdent (s, i1)::TCPar _::(Tsizeof _|TTilde _ | TString _)::_ , TOPar _::_ )
    when not_struct_enum before
      && ok_typedef s
    -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* [(,] xx [   AND parameterdeclaration *)
  | (TIdent (s, i1)::TOCro _::_, (TComma _ |TOPar _)::_)
      when (LP.current_context() = LP.InParameter) 
      && ok_typedef s
     -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)
        
  (*------------------------------------------------------------*)
  (* if 'x*y' maybe an expr, maybe just a classic multiplication *)
  (* but if have a '=', or ','   I think not *)
  (*------------------------------------------------------------*)

  (* static|const|... xx * yy    '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::_ , 
     (Tregister _|Tstatic _ |Tvolatile _|Tconst _|Trestrict _|Tvirtual _)::_) when
      ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

        
  (*  TODO  xx * yy ; AND in start of compound element  *)


  (*  xx * yy,      AND  in paramdecl   '*' or '&' *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s 
      -> 

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * &yy,      AND  in paramdecl *)
  | (TIdent (s, i1)::TMul _::TAnd _::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s 
      -> 

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy ;     AND in Toplevel, except when have = before  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TPtVirg _::_ , TEq _::_) ->
      TIdent (s, i1)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TPtVirg _::_ , _)
    when not_struct_enum before && 
      (LP.is_top_or_struct (LP.current_context ()))
      -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * yy ,     AND in Toplevel  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before 
      && (LP.is_top_or_struct (LP.current_context ()))
      (*(LP.current_context () = LP.InTopLevel)*)
      && ok_typedef s 
      -> 

      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * yy (     AND in Toplevel  '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(TIdent (_, i2))::TOPar _::_ , _)
    when not_struct_enum before  
      && (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * operator      AND in Toplevel  '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(Toperator (i2))::_ , _)
    when not_struct_enum before  
      && (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx *& yy (     AND in Toplevel *)
  | (TIdent (s, i1)::TMul _::TAnd _::(TIdent (s2, i2))::TOPar _::_ , _)
    when not_struct_enum before  
      && (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

        
  (* xx * yy [ *)
  (* todo? enough ? cos in struct def we can have some expression ! *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOCro _::_ , _)
    when not_struct_enum before && 
      (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s 
      -> 
      msg_typedef (s,i1);  LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* u16: 10; in struct *)
  | (TIdent (s, i1)::TCol _::_ , (TOBrace _ | TPtVirg _)::_)
    when 
      (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s 
      -> 
      msg_typedef (s,i1);  LP.add_typedef_root s;
      TypedefIdent (s, i1)
        

    (*  why need TOPar condition as stated in preceding rule ? really needed ? *)
    (*   YES cos at toplevel can have some expression !! for instance when *)
    (*   enter in the dimension of an array *)
    (*
      | (TIdent s::TMul::TIdent s2::_ , _)
      when (take_safe 1 !passed_tok <> [Tstruct] &&
      (take_safe 1 !passed_tok <> [Tenum]))
      &&
      !LP._lexer_hint = Some LP.Toplevel -> 
      msg_typedef (s,i1); 
      LP.add_typedef_root s;
      TypedefIdent s
     *)

  (*  xx * yy =  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TEq _::_ , _)
    when not_struct_enum before 
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)




  (*  xx * yy)      AND in paramdecl    '*' or '&' *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TCPar _::_ , _)
      when not_struct_enum before && (LP.current_context () = LP.InParameter)
        && ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx * &yy)      AND in paramdecl *)
  | (TIdent (s, i1)::TMul _::TAnd _::TIdent (s2, i2)::TCPar _::_ , _)
      when not_struct_enum before && (LP.current_context () = LP.InParameter)
      && ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)
          
  (*  xx * yy; *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , 
     (((TOBrace _| TPtVirg _)::_)
     (*c++ext: public: xx * y; *)
      | TCol _::(Tpublic _ |Tprotected _|Tprivate _)::_
    ))
       when not_struct_enum before 
      && ok_typedef s 
        ->
      msg_typedef (s,i1);  LP.add_typedef_root s;
      pr2 ("PB MAYBE: dangerous typedef inference, maybe not a typedef: " ^ s);
      TypedefIdent (s, i1)

  (* : public y *)
  | (TIdent (s, i1))::_ , (Tpublic _ |Tprotected _|Tprivate _)::TCol _::_
      when ok_typedef s 
      ->
      msg_typedef (s,i1);  LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*  xx * yy,  and ';' before xx *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , 
     (TOBrace _| TPtVirg _)::_) when
      ok_typedef s 
    ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (* xx_t * yy *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::_ , _)  
      when s ==~ regexp_typedef && not_struct_enum before 
        (* struct user_info_t sometimes *) 
      && ok_typedef s 
        -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** yy *)  (* wrong ? maybe look at position, if space then 
   * don't do it *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (* xx *** yy *)
  | (TIdent (s, i1)::TMul _::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before 
      && ok_typedef s 
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

  (*  xx ** ) *)
  | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
    when not_struct_enum before  
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      && ok_typedef s 
      ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)



  (* ----------------------------------- *)

  (*  (xx) yy    yy or int or char (and string??) *)
  | (TOPar info::TIdent (s, i1)::TCPar i2::
        (TIdent (_,i3)|TInt (_,i3)|TFloat(_,i3)|TChar(_,i3))::_ , 
    x::_)  
    when not (TH.is_stuff_taking_parenthized x) &&
      Ast.line_of_info i2 = Ast.line_of_info i3
      && ok_typedef s 
      -> 

      msg_typedef (s,i1); LP.add_typedef_root s;
      TOPar info

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)

  (*  (xx) (    yy) *)
  | (TOPar info::TIdent (s, i1)::TCPar _::TOPar _::_ , x::_)  
    when not (TH.is_stuff_taking_parenthized x)  
      && ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TOPar info

  (*  (xx * ) yy *)
  | (TOPar info::TIdent (s, i1)::TMul _::TCPar _::TIdent (s2, i2)::_ , _) when 
      ok_typedef s 
        -> 
      msg_typedef (s,i1); LP.add_typedef_root s;
      TOPar info

(* c++ext: interference with constructor recursive call,
   cf tests2/typedef_false_interference_constructor1.cpp
   could also add LP.lexer_hint.ctor_mem_initializer and
   no trigger following rule if in this region

  (* (xx){ ... }  constructor *)
  | (TIdent (s, i1)::TCPar _::TOBrace _::_ , TOPar _::x::_)  
      when (*s ==~ regexp_typedef && *) not (TH.is_stuff_taking_parenthized x) 
      && ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)

*)


        (* can have sizeof on expression
           | (Tsizeof::TOPar::TIdent s::TCPar::_,   _) -> 
           msg_typedef (s,i1); 
           LP.add_typedef_root s;
           Tsizeof
         *)
   (* x ( *y )(params),  function pointer *)
  | (TIdent (s, i1)::TOPar _::TMul _::TIdent _::TCPar _::TOPar _::_,  _) 
      when not_struct_enum before
      && ok_typedef s 
        ->
      msg_typedef (s,i1); LP.add_typedef_root s;
      TypedefIdent (s, i1)


  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | ((TIfdef ii |TIfdefelse ii |TIfdefelif ii |TEndif ii |
      TIfdefBool (_,ii)|TIfdefMisc(_,ii)|TIfdefVersion(_,ii))
        as x)
    ::_, _ 
      -> 
      if not !Flag.ifdef_to_if 
      then TCommentCpp (Token_cpp.CppDirective, ii)
      else 
        if not (LP.current_context () = LP.InTopLevel)
        then x
        else begin
          pr2_pp("IFDEF: or related outside function. I treat it as comment");
          TCommentCpp (Token_cpp.CppDirective, ii)
        end


  | (TUndef (id, ii) as x)::_, _ 
      -> 
        if (pass >= 2)
        then begin
          pr2_pp("UNDEF: I treat it as comment");
          TCommentCpp (Token_cpp.CppDirective, ii)
        end
        else x

  | (TCppDirectiveOther (ii) as x)::_, _ 
      -> 
        if (pass >= 2)
        then begin
          pr2_pp ("OTHER directive: I treat it as comment");
          TCommentCpp (Token_cpp.CppDirective, ii)
        end
        else x

   (* If ident contain a for_each, then certainly a macro. But to be
    * sure should look if there is a '{' after the ')', but it requires
    * to count the '('. Because this can be expensive, we do that only
    * when the token contains "for_each". 
    *)
(*c++ext: some false positif so for now commented 
soluce will be to check that InFunction, the not toplevel is not good
enough anymore with nested stuff.
  | (TIdent (s, i1)::TOPar _::rest, _) when not (LP.current_context () = LP.InTopLevel)  
      (* otherwise a function such as static void loopback_enable(int i) { 
       * will be considered as a loop 
       *)
        ->

 
      if s ==~ regexp_foreach && 
        is_really_foreach (Common.take_safe forLOOKAHEAD rest)
   
      then begin
        msg_foreach s;
        TMacroIterator (s, i1)
      end
      else TIdent (s, i1)
*)


                    
 (*-------------------------------------------------------------*)
 | v::xs, _ -> v
 | _ -> raise Impossible

let lookahead a b = 
  Common.profile_code "C parsing.lookahead" (fun () -> lookahead2 a b)
