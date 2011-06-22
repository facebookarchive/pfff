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
module TV = Token_views_cpp
module Parser = Parser_cpp

open Parser_cpp
open Token_views_cpp

open Parsing_hacks_lib

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO 
 * TIdent_TemplatenameInQualifier 
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let templateLOOKAHEAD = 30
  
(* note: no need to check for TCPar to stop for instance the search, 
 * this is will be done automatically because we would be inside a 
 * Parenthised expression.
 *)
let rec have_a_tsup_quite_close xs =
  match xs with
  | [] -> false
  | x::xs -> 
      (match x with
      | {t=TSup i} -> true

      (* false positive. pad:???? *)
      | {t=tok} when TH.is_static_cast_like tok -> false

      (* ugly: 
       * TODO: should probably detect if template '<' if the ident
       * just before the '<' has no space between
       *)
      | {t=(TOBrace _ | TPtVirg _ | TCol _ | TAssign _ )} -> 
          false

      | {t=TInf i} -> 
          (* probably nested template, still try
           * TODO: bug when have i < DEG<...>::foo(...)
           *  we should recurse!
           *)
          have_a_tsup_quite_close xs

      (* bugfix: but want allow some binary operator :) like '*' *)
      | {t=tok} when TH.is_binary_operator_except_star tok -> false

      | x -> have_a_tsup_quite_close xs
      )


(* precondition: there is a tsup 
 *)
let rec find_tsup_quite_close tok_open xs = 
  let rec aux acc xs =
    match xs with
    | [] -> 
        failwith (spf "PB: find_tsup_quite_close, no > for < at line %d"
                     (TH.line_of_tok tok_open.t))
    | x::xs -> 
        (match x with
        | {t=TSup ii} -> 
            List.rev acc, (x,ii), xs
              
        | {t=TInf ii} -> 
            (* recurse *)
            let (before, (tsuptok,_), after) = find_tsup_quite_close x xs in
            (* we don't care about this one, it will be eventually be 
             * transformed by the caller *)
            aux (tsuptok:: (List.rev before) ++(x::acc)) after
              
        | x -> aux (x::acc) xs
        )
  in
  aux [] xs



(* 
 * TODO: right now this is less useful because we actually
 *  comment template args in a previous pass, but at some point this
 *  will be useful.
*)
let rec filter_for_typedef multi_groups = 

  let _template_args = ref [] in

  (* remove template *)
  let rec aux xs =
    xs +> Common.map_filter (function
    | TV.Braces (t1, xs, t2) ->
        Some (TV.Braces (t1, aux xs, t2))
    | TV.Parens  (t1, xs, t2) ->
        Some (TV.Parens (t1, aux xs, t2))
    | TV.Angle (t1, xs, t2) ->
        (* todo: analayze xs!! add in _template_args 
         * todo: add the t1,t2 around xs to have
         *  some sentinel for the typedef heuristics patterns
         *  who often look for the token just before the typedef.
         *)
        None

    (* remove other noise for the typedef inference *)
    | TV.Tok t1 -> 
        match t1.TV.t with
        (* const is a strong signal for having a typedef, so why skip it?
         * because it forces to duplicate rules. We need to infer
         * the type anyway even when there is no const around.
         * todo? maybe could do a special pass first that infer typedef
         * using only const rules, and then remove those const so 
         * have best of both worlds.
         *)
        | Tconst _ | Tvolatile _
        | Trestrict _
          -> None

        | Tregister _ | Tstatic _ | Tauto _ | Textern _
        | Tunion _
          -> None

        (* let's transform all '&' into '*' *)
        | TAnd ii -> Some (TV.Tok (mk_token_extended (TMul ii)))

        (* and operator into TIdent 
         * TODO: skip the token just after the operator keyword?
         * could help some heuristics too
        *)
        | Toperator ii -> 
            Some (TV.Tok (mk_token_extended (TIdent ("operator", ii))))

        | _ -> Some (TV.Tok t1)
    )
  in
  let xs = aux multi_groups in
  (* todo: look also for _template_args *)
  [TV.tokens_of_multi_grouped xs]

let no_space_between i1 i2 =
  (Ast.line_of_info i1 = Ast.line_of_info i2) &&
  (Ast.col_of_info i1 + String.length (Ast.str_of_info i1))= Ast.col_of_info i2


let look_like_argument xs =
  xs +> List.exists (function
  | Tok {t=(TInt _ | TFloat _ | TChar _ | TString _) } -> true
  | Tok {t=(Ttrue _ | Tfalse _) } -> true
  | Tok {t=(Tnew _ )} -> true
  | Tok {t= tok} when TH.is_binary_operator_except_star tok -> true
  | Tok {t = (TDot _ | TPtrOp _ | TPtrOpStar _ | TDotStar _);_} -> true
  | _ -> false
  )

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

(* note: some macros in standard.h may expand to static_cast, so perhaps
 * better to do template detection after macro expansion ?
 * 
 * C-s for TInf_Template in the grammar and you will see all cases
 * should be covered.
 *)
let find_template_inf_sup xs =
  let rec aux xs =
  match xs with
  | [] -> ()

  (* template<...> *)
  | {t=Ttemplate _}::({t=TInf i2} as tok2)::xs ->
      change_tok tok2 (TInf_Template i2);
      let (before_sup, (toksup, toksupi), rest) = 
        find_tsup_quite_close tok2 xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_sup;
      aux rest

  (* static_cast *)
  | {t=tok1}::({t=TInf i2} as tok2)::xs
    when TH.is_static_cast_like tok1 -> 
      change_tok tok2 (TInf_Template i2);
      let (before_sup, (toksup, toksupi), rest) = 
        find_tsup_quite_close tok2 xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_sup;
      aux rest

  (* 
   * TODO: have_a_tsup_quite_close does not handle a relational < followed
   *  by a regular template.
  *)
  | {t=TIdent (s,i1)}::({t=TInf i2} as tok2)::xs
    when
      no_space_between i1 i2 && (* safe guard, and good style anyway *)
      have_a_tsup_quite_close (Common.take_safe templateLOOKAHEAD xs) 
     -> 
      change_tok tok2 (TInf_Template i2);
      let (before_sup, (toksup, toksupi), rest) = 
        find_tsup_quite_close tok2 xs in
      change_tok toksup (TSup_Template toksupi);

      (* old: was chaning to TIdent_Templatename but now first need
       * to do the typedef inference and then can transform the
       * TIdent_Typedef into a TIdent_Templatename
       *)
      
      (* recurse *)
      aux before_sup;
      aux rest

  (* recurse *)
  | x::xs -> aux xs

  in
  aux xs


let reclassify_tokens_before_idents_or_typedefs xs =
  let groups = List.rev xs in
  
  let rec aux xs =
    match xs with
    | [] -> ()

    (* xx::yy     where yy is ident (funcall, variable, etc)
     * need to do that recursively! if have a::b::c
     *)
    | Tok{t=TIdent _ | TIdent_ClassnameInQualifier _}
      ::Tok{t=TColCol _}
      ::Tok({t=TIdent (s2, i2)} as tok2)::xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier (s2, i2));
        aux ((Tok tok2)::xs)

    (* xx::t      wher et is a type 
     * TODO need to do that recursively! if have a::b::c
     *)
    | Tok{t=TIdent_Typedef _}::Tok({t=TColCol icolcol} as tcolcol)
      ::Tok({t=TIdent (s2, i2)} as tok2)::xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier_BeforeTypedef (s2, i2));
        change_tok tcolcol (TColCol_BeforeTypedef icolcol);
        aux xs

    (* xx::t<...> where t is a templatename *)
    | Tok{t=TIdent_Templatename _}::Tok({t=TColCol icolcol} as tcolcol)
      ::Tok({t=TIdent (s2, i2)} as tok2)::xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier_BeforeTypedef (s2, i2));
        change_tok tcolcol (TColCol_BeforeTypedef icolcol);
        aux xs

    (* t<...>    where t is a typedef *)
    | Angle (t1, xs_angle, t2)::Tok({t=TIdent_Typedef (s1, i1)} as tok1)::xs ->
        aux xs_angle;
        change_tok tok1 (TIdent_Templatename (s1, i1));
        (* recurse with tok1 too! *)
        aux (Tok tok1::xs)

    | x::xs -> 
        (match x with
        | Tok _ -> ()
        | Braces (t1, xs, t2)
        | Parens (t1, xs, t2)
        | Angle (t1, xs, t2)
          -> aux (List.rev xs)
        );
        aux xs
  in
  aux groups;
  ()


(* quite similar to filter_for_typedef 
 * TODO: at some point need have to remove this and instead
 *  have a correct filter_for_typedef that also returns
 *  nested types in template arguments (and some
 *  typedef heuristics that work on template_arguments too)
 * 
 * TODO: once you don't use it, remove certain grammar rules (C-s TODO)
 *)
let find_template_commentize groups =
  (* remove template *)
  let rec aux xs =
    xs +> List.iter (function
    | TV.Braces (t1, xs, t2) ->
        aux xs
    | TV.Parens  (t1, xs, t2) ->
        aux xs
    | TV.Angle (t1, xs, t2) as angle ->
        (* let's commentize everything *)
        [angle] +> TV.iter_token_multi (fun tok ->
          change_tok tok 
            (TComment_Cpp (Token_cpp.CplusplusTemplate, TH.info_of_tok tok.t))
        )

    | TV.Tok tok ->
        (* todo? should also pass the static_cast<...> which normally
         * expect some TInf_Template after. Right mow I manage
         * that by having some extra rules in the grammar
         *)
        (match tok.t with
        | Ttemplate _ ->
            change_tok tok 
              (TComment_Cpp (Token_cpp.CplusplusTemplate, TH.info_of_tok tok.t))
        | _ -> ()
        )
    )
  in
  aux groups

(* assumes a view without:
 * - template arguments
 * 
 * TODO: once you don't use it, remove certain grammar rules (C-s TODO)
 * 
 * note: passing qualifiers is slightly less important than passing template
 * arguments because they are before the name (as opposed to templates
 * which are after) and most of our heuristics for typedefs
 * look tokens forward, not backward (actually a few now look backward too)
 *)
let find_qualifier_commentize xs =
  let rec aux xs =
    match xs with
    | [] -> ()

    | ({t=TIdent _} as t1)::({t=TColCol _} as t2)::xs ->
        [t1; t2] +> List.iter (fun tok ->
          change_tok tok 
            (TComment_Cpp (Token_cpp.CplusplusQualifier, TH.info_of_tok tok.t))
        );
        aux xs

    (* recurse *)
    | x::xs ->
        aux xs
  in
  aux xs


(* assumes a view without: 
 * - template arguments, qualifiers, 
 * - comments and cpp directives 
 * - TODO public/protected/... ?
 *)
let set_context_tag groups =
  let rec aux xs =
  match xs with
  | [] -> ()
  (* class Foo { *)
  | Tok{t=Tclass _ | Tstruct _;_}::Tok{t=TIdent(s,_);_}
    ::(Braces(t1, body, t2) as braces)::xs
    ->
      [braces] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InClassStruct s)::tok.TV.where;
      );
      aux (braces::xs)

  (* class Foo : ... { *)

  | Tok{t=Tclass _ | Tstruct _;_}::Tok{t=TIdent(s,_);_}
    ::Tok{t= TCol _}::xs
    ->
      let (before, braces, after) =
        xs +> Common.split_when (function
        | Braces _ -> true
        | _ -> false
        )
      in
      aux before;
      [braces] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InClassStruct s)::tok.TV.where;
      );
      aux after


  (* need look what was before? look for a ident? *)
  | (Parens(t1, body, t2) as parens)::xs ->
      (* split at TComma? *)
      (if look_like_argument body
      then [parens] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InArgument)::tok.TV.where;
      )
      else
        (* TODO? look_like_parameter ? 
         * else? could be a cast too ... or what else?
        *)
          ()
      );
      aux xs
      

  | x::xs ->
      (match x with
      | Tok t -> ()
      | Parens (t1, xs, t2)
      | Braces (t1, xs, t2)
      | Angle  (t1, xs, t2)
         ->
          aux xs
      );
      aux xs
  in
  aux groups


(* assumes a view where:
 * - set_context_tag has been called.
 * TODO: filter the 'explicit' keyword?
 *)
let find_constructor xs =
  let rec aux xs = 
  match xs with
  | [] -> ()

  (* { Foo(... *)
  | {t=(TOBrace _ | TCBrace _ | TPtVirg _ | Texplicit _);_}
    ::({t=TIdent (s1, i1); where=(TV.InClassStruct s2)::_; _} as tok1)
    ::{t=TOPar _}::xs when s1 = s2 ->
      change_tok tok1 (TIdent_Constructor(s1, i1));
      aux xs

  (* public: Foo(...   could also filter the privacy directives so 
   * need only one rule
   *)
  | {t=(Tpublic _ | Tprotected _ | Tprivate _)}::{t=TCol _}
    ::({t=TIdent (s1, i1); where=(TV.InClassStruct s2)::_; _} as tok1)
    ::{t=TOPar _}::xs when s1 = s2 ->
      change_tok tok1 (TIdent_Constructor(s1, i1));
      aux xs

  (* recurse *)
  | x::xs -> aux xs
  in
  aux xs

(* assumes a view where:
 * - template have been filtered but NOT the qualifiers!
 *)
let find_constructor_outside_class xs =
  let rec aux xs =
    match xs with
    | [] -> ()

    | {t=TIdent (s1, i1);_}::{t=TColCol _}::({t=TIdent (s2,i2);_} as tok)::xs 
      when s1 = s2 ->
        change_tok tok (TIdent_Constructor (s2, i2));
        aux (tok::xs)
                        

    (* recurse *)
    | x::xs -> aux xs
  in
  aux xs



(* assumes have:
 * - the typedefs
 * - the right context
 *)
let find_constructed_object_and_more xs =
  let rec aux xs =
    match xs with
    | [] -> ()

    | {t=(Tdelete _| Tnew _);_}
      ::({t=TOCro i1} as tok1)::({t=TCCro i2} as tok2)::xs ->
        change_tok tok1 (TOCro_new i1);
        change_tok tok2 (TCCro_new i2);
        aux xs
        

    | {t=TIdent_Typedef _;_}::{t=TIdent _;_}::
        ({t=TOPar (ii);where=InArgument::_;_} as tok1)::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
          aux xs

    (* recurse *)
    | x::xs -> aux xs
  in
  aux xs
