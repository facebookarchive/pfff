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

      | {t=(TOBrace _ | TPtVirg _ )} -> 
          false

      (* bugfix: but want allow some binary operator :) like '*' *)
      | {t=tok} when TH.is_binary_operator_except_star tok -> false

      | x -> have_a_tsup_quite_close xs
      )


(* precondition: there is a tsup *)
let rec find_tsup_quite_close xs = 
  let rec aux acc xs =
    match xs with
    | [] -> failwith "PB: find_tsup_quite_close, no tsup"
    | x::xs -> 
        (match x with
        | {t=TSup ii} -> 
            acc, (x,ii), xs
              
        | {t=TInf ii} -> 
            (* recurse *)
            let (before, (tsuptok,_), after) = find_tsup_quite_close xs in
            (* we don't care about this one, it will be eventually be 
             * transformed by the caller *)
            aux (x::(before++[tsuptok])) xs
              
        | x -> aux (x::acc) xs
        )
  in
  let (before, tsup, after) = aux [] xs in
  List.rev before, tsup, after




let rec filter_for_typedef xs = 
  let xs =
  xs +> Common.exclude (fun tok_ext ->
    match tok_ext.TV.t with
   (* const is a strong signal for having a typedef, so why skip it?
    * because it forces to duplicate rules. We need to infer
    * the type anyway even when there is no const around.
    * todo? maybe could do a special pass first that infer typedef
    * using only const rules, and then remove those const so 
    * have best of both worlds.
    *)
    | Tconst _ | Tvolatile _
    | Trestrict _
      -> true
    | _ -> false
  )
  in
  let groups = TV.mk_multi xs in
  filter_qualifier_and_template groups

and filter_qualifier_and_template xs =
  let _template_args = ref [] in

  (* remove template *)
  let rec aux xs =
    xs +> Common.map_filter (function
    | TV.Braces (t1, xs, t2) ->
        Some (TV.Braces (t1, aux xs, t2))
    | TV.Parens  (t1, xs, t2) ->
        Some (TV.Parens (t1, aux xs, t2))
    | TV.Angle (t1, xs, t2) ->
        (* todo: analayze xs!! add in _template_args *)
        None
    | TV.Tok t1 -> Some (TV.Tok t1)
    )
  in
  
  let xs = aux xs in
  (* todo: look also for _template_args *)
  [TV.tokens_of_multi_grouped xs]

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

(* note: some macros in standard.h may expand to static_cast, so perhaps
 * better to do template detection after macro expansion ?
 *)
let find_template_inf_sup xs =
  let rec aux xs =
  match xs with
  | [] -> ()

  (* static_cast *)
  | {t=tok1}::({t=TInf i2} as tok2)::xs
    when TH.is_static_cast_like tok1 -> 
      change_tok tok2 (TInf_Template i2);
      let (before_inf, (toksup, toksupi), rest) = find_tsup_quite_close xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_inf;
      aux rest

  | {t=TIdent (s,i1)}::({t=TInf i2} as tok2)::xs
    when have_a_tsup_quite_close (Common.take_safe templateLOOKAHEAD xs) -> 
      change_tok tok2 (TInf_Template i2);
      let (before_inf, (toksup, toksupi), rest) = find_tsup_quite_close xs in
      change_tok toksup (TSup_Template toksupi);

      (* old: was chaning to TIdent_Templatename but now first need
       * to do the typedef inference and then can transform the
       * TIdent_Typedef into a TIdent_Templatename
       *)
      
      (* recurse *)
      aux before_inf;
      aux rest

  (* recurse *)
  | x::xs -> aux xs

  in
  aux xs


let reclassify_tokens_before_idents_or_typedefs xs =
  let groups = List.rev (TV.mk_multi xs) in
  
  let rec aux xs =
    match xs with
    | [] -> ()

    (* xx::yy     where yy is ident (funcall, variable, etc)  *)
    | Tok{t=TIdent _}::Tok{t=TColCol _}
      ::Tok({t=TIdent (s2, i2)} as tok2)::xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier (s2, i2));
        aux xs

    (* xx::t      wher et is a type *)
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

