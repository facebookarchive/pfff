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

open Lexer_parser_cpp (* for the fields of lexer_hint type *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module tries to detect some cpp idioms so that we can parse as-is
 * files by adjusting or commenting some tokens.
 * Sometime we use some indentation information,  sometimes we
 * do some kind of lalr(k) by finding patterns. We often try to
 * work on a better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so that we can pattern match more easily
 * complex cpp idiom pattern (cf token_views_cpp.ml).
 * We also try to get more contextual information such as whether the
 * token is in an initializer because many patterns are different
 * depending on context.
 * 
 * Example of cpp idioms:
 *  - if 0 for commenting stuff (not always code, sometimes just real comments)
 *  - ifdef old version
 *  - ifdef funheader
 *  - ifdef statements, ifdef expression, ifdef-mid
 *  - macro toplevel (with or without a trailing ';')
 *  - macro foreach
 *  - macro higher order
 *  - macro declare
 *  - macro debug
 *  - macro no ';'
 *  - macro string, and macro function string taking param and ##
 *  - macro attribute
 * 
 * Cf the TMacroXxx in parser_c.mly and MacroXxx in ast_c.ml
 * 
 * We also try to infer typedef.
 * 
 * c++ext: We also try to infer templates, constructors, etc.
 * 
 * We also do other stuff involving cpp like expanding macros,
 * and we try parse define body by finding the end of define virtual 
 * end-of-line token. But now most of the code is actually in pp_token.ml
 * It is related to what is in the yacfe configuration file (e.g. standard.h)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_pp_stuff xs = 
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

let not_struct_enum = function
  | x::xs when TH.is_struct_like_keyword x -> false
  | [] -> true
  | x::xs -> true

(*****************************************************************************)
(* Fix tokens_cpp *)
(*****************************************************************************)

(* 
 * Main entry point for the token reclassifier (which generates "fresh" tokens).
 * 
 * The order of the rules is important, if you put the action heuristic first,
 * then because of ifdef, can have not closed paren
 * and so may believe that higher order macro 
 * and it will eat too much tokens. So important to do 
 * first the ifdef heuristic.
 * 
 * I recompute multiple times cleaner cos the mutable
 * can have be changed and so may have more comments
 * in the token original list.
 *)
let fix_tokens_cpp2 ~macro_defs tokens = 
  let tokens2 = ref (tokens +> acc_map mk_token_extended) in
  
  begin 

    (* ifdef *)
    let cleaner = !tokens2 +> List.filter (fun x -> 
      not (TH.is_comment x.tok) (* could filter also #define/#include *)
    ) in
    let ifdef_grouped = mk_ifdef cleaner in
    find_ifdef_funheaders ifdef_grouped;
    find_ifdef_bool       ifdef_grouped;
    find_ifdef_mid        ifdef_grouped;


    (* macro 1 *)
    let cleaner = !tokens2 +> filter_pp_stuff in

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
    let cleaner = !tokens2 +> filter_pp_stuff in

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
    let cleaner = !tokens2 +> filter_pp_stuff in
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
let lookahead2 next before = 
 (* TODO *)
 let pass = 2 in

  match (next, before) with

  (* c++ext: constructed objects part2 *)

  (* > xx(   and in function *)
  | TOPar i1::_,           TIdent(s,i2)::TSup_Template _::_ 
    when (LP.current_context () = (LP.InFunction)) -> 
      fresh_tok (TOPar_CplusplusInit i1)

  (* yy xx(   and in function. The TIdent_Typedef here comes from
   * a rewriting done in parsing_hacks_cpp.ml.
   * todo? enough?
   *)
  | TOPar i1::_,              TIdent(s,i2)::TIdent_Typedef _::_ 
    when (LP.current_context () = (LP.InFunction)) -> 
      fresh_tok (TOPar_CplusplusInit i1)

  (* int xx( and in function, problably not a internal declaration *)
  | TOPar i1::_,   TIdent(s,i2)::t1::_ 
    when (LP.current_context () = (LP.InFunction)) && TH.is_basic_type t1 -> 
      fresh_tok (TOPar_CplusplusInit i1)

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
  | (TIdent_Typedef(s,i1))::TOPar _::_ , ((Ttypedef _|Tnew _ |TCPar _)::_) ->
      TIdent_Typedef(s,i1)

  | tok::TOPar _::_ , ((Ttypedef _|Tnew _ |TCPar _)::_) 
    when TH.is_basic_type tok ->
      tok

  (* addon: inline XXX() *)
  | ((TIdent(s,i1)|TIdent_Typedef(s,i1))::TOPar _::_, 
    (((TOBrace _| TCBrace _|TPtVirg _ | Texplicit _  |Tinline _)::_)
     (* for    public:   xx * y; *)
      | TCol _::(Tpublic _ |Tprotected _|Tprivate _)::_
    )) 
    when (LP.current_context () = (LP.InClassStruct s)) -> 
      fresh_tok (TIdent_Constructor(s,i1))

  | (TIdent_Typedef(s,i1))::TOPar _::_ , _ -> 
      fresh_tok (TIdent_TypedefConstr(s,i1))


  (* basic type can now also be used as "cast_function/constructor" *)
  | (Tchar(i1))::TOPar _::_ , _  
    when (LP.current_context() <> LP.InParameter) ->
      fresh_tok (Tchar_Constr(i1))
  | (Tint(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tint_Constr(i1))
  | (Tfloat(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tfloat_Constr(i1))
  | (Tdouble(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tdouble_Constr(i1))
  | (Tshort(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tshort_Constr(i1))
  | (Tlong(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tlong_Constr(i1))
  | (Tbool(i1))::TOPar _::_ , _   
    when (LP.current_context() <> LP.InParameter) -> 
      fresh_tok (Tbool_Constr(i1))




  (* new foo() *)
  | TIdent(s,i1)::_,   Tnew _::_ ->
      fresh_tok (TIdent_Typedef(s, i1))

  (* template<xxx  on xxx *)
  | TIdent(s,i1)::_,      TInf_Template _::_ ->
      fresh_tok (TIdent_Typedef(s, i1))

  (* template<xxx, yyy  on yyy *)
  | TIdent(s,i1)::_,      TComma _::TIdent_Typedef(_)::TInf_Template _::_ -> 
      fresh_tok (TIdent_Typedef(s, i1))

  (*-------------------------------------------------------------*)
  (* typedef inference, parse_typedef_fix3 *)
  (*-------------------------------------------------------------*)
  (* TODO: use a fix_tokens_typedef instead of lookahead. Need 
   * complex context? Need improve set_context then ...
   *)

  (* xx xx *)
  | (TIdent(s,i1)::TIdent(s2,i2)::_ , _) 
    when not_struct_enum before && s = s2 && ok_typedef s ->
      (* (take_safe 1 !passed_tok <> [TOPar]) ->  *)
      (* parse_typedef_fix3:
       *    acpi_object         acpi_object;
       * etait mal pars'e, car pas le temps d'appeler dt()  dans le type_spec. 
       * Le parser en interne a deja appel'e le prochain token pour pouvoir
       * decider des choses.
       *  => special case in lexer_heuristic, again
       *)
      if !Flag.debug_typedef 
      then pr2 ("TYPEDEF: disable typedef cos special case: " ^ s); 
      LP.disable_typedef();

      fresh_tok (TIdent_Typedef(s, i1))

  (* xx yy (c++ext: or operator) *)
  | (TIdent (s, i1)::(TIdent (_, _) | Toperator _)::_  , _) 
    when not_struct_enum before  && ok_typedef s ->
         (* && not_annot s2 BUT lead to false positive*)
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx inline *)
  | (TIdent (s, i1)::Tinline i2::_  , _) 
    when not_struct_enum before && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (* [,(] xx [,)] AND param decl *)
  | (TIdent (s, i1)::(TComma _|TCPar _)::_ , (TComma _ |TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
     && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (* [,(] xx =  AND param decl  for c++ext:  *)
  | (TIdent (s, i1)::TEq _ ::_ , (TComma _|TOPar _)::_ )
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
      && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)
  (* xx* [,)]    '*' or '&' *)
  (* specialcase:  [,(] xx* [,)] *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before && ok_typedef s ->
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))


(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)
  (* xx** [,)] *)
  (* specialcase:  [,(] xx** [,)] *)
  | (TIdent (s, i1)::TMul _::TMul _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before && ok_typedef s ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

  (* specialcase:  xx*& [,)] *)
  | (TIdent (s, i1)::TMul _::TAnd _::(TComma _|TCPar _)::_ , (*(TComma _|TOPar _)::*)_ )
    when not_struct_enum before && ok_typedef s ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))


  (* just look for const xx ? no!! cos can have char const xx; *)

  (* const xx *)
  | (TIdent (s, i1))::xs,  Tconst _::_ 
     when (LP.current_context() = LP.InTemplateParam) && ok_typedef s  ->
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx const *   USELESS because of next rule ? *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::TMul _::_ , _ ) 
    when not_struct_enum before && ok_typedef s ->
    (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))
  
  (* xx const *)
  | (TIdent (s, i1)::(Tconst _|Tvolatile _|Trestrict _)::_ , _ ) 
    when not_struct_enum before && ok_typedef s ->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx * const *)
  | (TIdent (s, i1)::TMul _::(Tconst _ | Tvolatile _|Trestrict _)::_ , _ ) 
    when not_struct_enum before && ok_typedef s->
      (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)

  (* ( const xx)  *)
  | (TIdent (s, i1)::TCPar _::_,  (Tconst _ | Tvolatile _|Trestrict _)::TOPar _::_) 
   when ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (* ( xx ) [sizeof, ~]  c++ext: TString *)
  | (TIdent (s, i1)::TCPar _::(Tsizeof _|TTilde _ | TString _)::_ , TOPar _::_ )
    when not_struct_enum before && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (* [(,] xx [   AND parameterdeclaration *)
  | (TIdent (s, i1)::TOCro _::_, (TComma _ |TOPar _)::_)
    when (LP.current_context() = LP.InParameter) && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))
        
  (* 
   * If 'x*y' maybe an expr, maybe just a classic multiplication
   * but if have a '=', or ','   I think not 
   *)

  (* static|const|... xx * yy    '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::_ , 
     (Tregister _|Tstatic _ |Tvolatile _|Tconst _|Trestrict _|Tvirtual _)::_) 
    when ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))
        
  (*  TODO  xx * yy ; AND in start of compound element  *)

  (*  xx * yy,      AND  in paramdecl   '*' or '&' *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
    && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * &yy,      AND  in paramdecl *)
  | (TIdent (s, i1)::TMul _::TAnd _::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before && (LP.current_context() = LP.InParameter)
     && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * yy ;     AND in Toplevel, except when have = before  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TPtVirg _::_ , TEq _::_)
    ->
      TIdent (s, i1)

  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TPtVirg _::_ , _)
    when not_struct_enum before && (LP.is_top_or_struct (LP.current_context ()))
      ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * yy ,     AND in Toplevel  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TComma _::_ , _)
    when not_struct_enum before 
     && (LP.is_top_or_struct (LP.current_context ()))
      (*(LP.current_context () = LP.InTopLevel)*)
     && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * yy (     AND in Toplevel  '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(TIdent (_, i2))::TOPar _::_ , _)
    when not_struct_enum before  
      && (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * operator      AND in Toplevel  '*' or '&'  *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::(Toperator (i2))::_ , _)
    when not_struct_enum before  
      && (LP.is_top_or_struct (LP.current_context ()))
      && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx *& yy (     AND in Toplevel *)
  | (TIdent (s, i1)::TMul _::TAnd _::(TIdent (s2, i2))::TOPar _::_ , _)
    when not_struct_enum before  
     && (LP.is_top_or_struct (LP.current_context ()))
     && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx * yy [ *)
  (* todo? enough ? cos in struct def we can have some expression ! *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TOCro _::_ , _)
    when not_struct_enum before && 
     (LP.is_top_or_struct (LP.current_context ()))
     && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

  (* u16: 10; in struct *)
  | (TIdent (s, i1)::TCol _::_ , (TOBrace _ | TPtVirg _)::_)
    when (LP.is_top_or_struct (LP.current_context ())) && ok_typedef s -> 
      fresh_tok (TIdent_Typedef (s, i1))

    (* Why need TOPar condition as stated in preceding rule ? really needed ?
     * YES cos at toplevel can have some expression !! for instance when
     * enter in the dimension of an array
     *)
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
    when not_struct_enum before && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * yy)      AND in paramdecl    '*' or '&' *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::TCPar _::_ , _)
     when not_struct_enum before && (LP.current_context () = LP.InParameter)
      && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * &yy)      AND in paramdecl *)
  | (TIdent (s, i1)::TMul _::TAnd _::TIdent (s2, i2)::TCPar _::_ , _)
    when not_struct_enum before && (LP.current_context () = LP.InParameter)
    && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))
          
  (*  xx * yy; *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TPtVirg _::_ , 
     (((TOBrace _| TPtVirg _)::_)
     (*c++ext: public: xx * y; *)
      | TCol _::(Tpublic _ |Tprotected _|Tprivate _)::_
    ))
    when not_struct_enum before && ok_typedef s ->
      pr2 ("PB MAYBE: dangerous typedef inference, maybe not a typedef: " ^ s);
      fresh_tok (TIdent_Typedef (s, i1))

  (* : public y *)
  | (TIdent (s, i1))::_ , (Tpublic _ |Tprotected _|Tprivate _)::TCol _::_
    when ok_typedef s  ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx * yy,  and ';' before xx *) (* wrong ? *)
  | (TIdent (s, i1)::TMul _::TIdent (s2, i2)::TComma _::_ , 
     (TOBrace _| TPtVirg _)::_) 
    when ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx_t * yy *)
  | (TIdent (s, i1)::(TMul _|TAnd _)::TIdent (s2, i2)::_ , _)  
    when s ==~ regexp_typedef && not_struct_enum before && ok_typedef s ->
    (* struct user_info_t sometimes *) 
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx ** yy *)  (* wrong ? maybe look at position, if space then 
   * don't do it *)
  | (TIdent (s, i1)::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before && ok_typedef s  ->
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

  (* xx *** yy *)
  | (TIdent (s, i1)::TMul _::TMul _::TMul _::TIdent (s2, i2)::_ , _)
    when not_struct_enum before && ok_typedef s ->
        (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

  (*  xx ** ) *)
  | (TIdent (s, i1)::TMul _::TMul _::TCPar _::_ , _)
    when not_struct_enum before  && ok_typedef s ->
    (* && !LP._lexer_hint = Some LP.ParameterDeclaration *)
      fresh_tok (TIdent_Typedef (s, i1))

  (* ----------------------------------- *)

  (*  (xx) yy    yy or int or char (and string??) *)
  | (TOPar info::TIdent (s, i1)::TCPar i2::
        (TIdent (_,i3)|TInt (_,i3)|TFloat(_,i3)|TChar(_,i3))::_ , 
    x::_)  
    when not (TH.is_stuff_taking_parenthized x) &&
      Ast.line_of_info i2 = Ast.line_of_info i3
      && ok_typedef s -> 

      (* ??? msg_typedef (s,i1); LP.add_typedef_root s; *)
      TOPar info

(*TODO can add static_cast<> case here too, look for TSup not just TCPar *)

  (*  (xx) (    yy) *)
  | (TOPar info::TIdent (s, i1)::TCPar _::TOPar _::_ , x::_)  
    when not (TH.is_stuff_taking_parenthized x)  
      && ok_typedef s 
        ->
      (* ??? msg_typedef (s,i1); LP.add_typedef_root s; *)
      TOPar info

  (*  (xx * ) yy *)
  | (TOPar info::TIdent (s, i1)::TMul _::TCPar _::TIdent (s2, i2)::_ , _) when 
      ok_typedef s 
        -> 
      (* ??? msg_typedef (s,i1); LP.add_typedef_root s; *)
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
    when not_struct_enum before && ok_typedef s ->
      fresh_tok (TIdent_Typedef (s, i1))

  (*-------------------------------------------------------------*)
  (* CPP *)
  (*-------------------------------------------------------------*)
  | ((TIfdef ii |TIfdefelse ii |TIfdefelif ii |TEndif ii |
      TIfdefBool (_,ii)|TIfdefMisc(_,ii)|TIfdefVersion(_,ii))
        as x) ::_, _  -> 

      if not !Flag.ifdef_to_if 
      then fresh_tok (TComment_Cpp (Token_cpp.CppDirective, ii))
      else 
        if not (LP.current_context () = LP.InTopLevel)
        then x
        else fresh_tok (TComment_Cpp (Token_cpp.CppDirective, ii))

  | (TUndef (id, ii) as x)::_, _ -> 
      if (pass >= 2)
      then fresh_tok (TComment_Cpp (Token_cpp.CppDirective, ii))
      else x

  | (TCppDirectiveOther (ii) as x)::_, _ -> 
        if (pass >= 2)
        then fresh_tok (TComment_Cpp (Token_cpp.CppDirective, ii))
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
