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
module PI = Parse_info

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

let no_space_between i1 i2 =
  (PI.line_of_info i1 = PI.line_of_info i2) &&
  (PI.col_of_info i1 + String.length (PI.str_of_info i1))= PI.col_of_info i2

(*****************************************************************************)
(* Template inference *)
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

      (* false positive *)
      | {t=tok} when TH.is_static_cast_like tok -> false

      (* ugly: *)
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


(* precondition: there is a tsup *)
let rec find_tsup_quite_close tok_open xs = 
  let rec aux acc xs =
    match xs with
    | [] -> 
        raise (UnclosedSymbol 
                  (spf "PB: find_tsup_quite_close, no > for < at line %d"
                     (TH.line_of_tok tok_open.t)))
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


(* note: some macros in standard.h may expand to static_cast, so perhaps
 * better to do template detection after macro expansion ?
 * 
 * C-s for TInf_Template in the grammar and you will see all cases
 * should be covered by the patterns below.
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

  (* static_cast<...> *)
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

      (* old: was changing to TIdent_Templatename but now first need
       * to do the typedef inference and then can transform the
       * TIdent_Typedef into a TIdent_Templatename
       *)
      
      (* recurse *)
      aux before_sup;
      aux rest

  (* special cases which allow extra space between ident and <
   * but I think it would be better for people to fix their code
   * | {t=TIdent (s,i1)}::({t=TInf i2} as tok2)
   *  ::tok3::({t=TSup i4} as tok4)::xs ->
   *  ...
   * 
   *)

  (* recurse *)
  | x::xs -> aux xs

  in
  aux xs


(*****************************************************************************)
(* Parameter/Argument inference *)
(*****************************************************************************)

let look_like_argument tok_before xs =

  let xs = xs +> List.map (function
    | Tok ({t=TAnd ii} as record) -> Tok ({record with t=TMul ii})
    | x -> x
  )
  in

  (* split by comma so can easily check if have stuff like '*xx'
   * that takes the full argument
   *)
  let xxs = split_comma xs in

  let aux1 xs =
    match xs with
    | [] -> false

    (* *xx *)
    | [Tok{t=TMul _}; Tok{t=TIdent _}] -> true

    (* *(xx) *)
    | [Tok{t=TMul _}; Parens _] -> true

    (* TODO: xx * yy  and space = 1 between the 2 :) *)

    | _ -> false
  in

  
  let rec aux xs =
    match xs with
    | [] -> false
    (* a function call probably *)
    | Tok{t=TIdent _}::Parens _::xs -> 
        (* todo? look_like_argument recursively in Parens || aux xs ? *)
        true
    (* if have = ... then must stop, could be default parameter
     * of a method
     *)
    | Tok{t=TEq _}::xs ->
        false

    (* could be part of a type declaration *)
    | Tok {t=TOCro _}::Tok {t=TCCro _}::xs -> false

    | x::xs ->
        (match x with
        | Tok {t=(TInt _ | TFloat _ | TChar _ | TString _) } -> true
        | Tok {t=(Ttrue _ | Tfalse _) } -> true
        | Tok {t=(Tthis _)} -> true
        | Tok {t=(Tnew _ )} -> true
        | Tok {t= tok} when TH.is_binary_operator_except_star tok -> true
        | Tok {t=(TInc _ | TDec _)} -> true
        | Tok {t = (TDot _ | TPtrOp _ | TPtrOpStar _ | TDotStar _);_} -> true
        | Tok {t = (TOCro _)} -> true
        | Tok {t = (TWhy _ | TBang _)} -> true
        | _ -> aux xs
        )
  in
  (* todo? what if they contradict each other? if one say arg and
   * the other a parameter?
   *)
  xxs +> List.exists aux1 || aux xs

(* todo: pass1, look for const, etc
 * todo: pass2, look xx_t, xx&, xx*, xx**, see heuristics in typedef
 * 
 * Many patterns should mimic some heuristics in parsing_hack_typedef.ml
 *)
let look_like_parameter tok_before xs =
  let xs = xs +> List.map (function
    | Tok ({t=TAnd ii} as record) -> Tok ({record with t=TMul ii})
    | x -> x
  )
  in

  let xxs = split_comma xs in

  let aux1 xs =
    match xs with
    | [] -> false

    | [Tok {t=TIdent (s, _)}] when s =~ ".*_t$" -> true
    (* with DECLARE_BOOST_TYPE, but have some false positives
     * when people do xx* indexPtr = const_cast<>(indexPtr);
     * | [Tok {t=TIdent (s, _)}] when s =~ ".*Ptr$" -> true
     *)
    (* ugly!! *)
    | [Tok {t=TIdent (s, _)}] when s = "StringPiece" -> true

    (* xx* *)
    | [Tok {t=TIdent _}; Tok {t=TMul _}] -> true

    (* xx** *)
    | [Tok {t=TIdent _}; Tok {t=TMul _}; Tok {t=TMul _}] -> true

    (* xx * y   
     * TODO could be multiplication (or xx & yy)
     * TODO? could look if space around :) but because of the
     *  filtering of template and qualifier the no_space_between
     *  may not work here. May need lower level access to the list
     *  of TCommentSpace and their position.
     * 
     * C-s for parameter_decl in grammar to see that catch() is
     * a InParameter.
     *)
    | [Tok {t=TIdent _}; Tok {t=TMul _};Tok {t=TIdent _};] ->
      (match tok_before with 
      | Tok{t=(
            TIdent _ 
          | Tcatch _ 
          | TAt_catch _
          (* ugly: TIdent_Constructor interaction between past heuristics *)
          | TIdent_Constructor _
          | Toperator _
        )} -> true 
      | _ -> false
      )

    | _ -> false
  in

  let rec aux xs =
    match xs with
    | [] -> false

    (* xx yy *)
    | Tok {t=TIdent _}::Tok{t=TIdent _}::xs -> true

    | x::xs ->
        (match x with
        | Tok {t= tok} when TH.is_basic_type tok -> true
        | Tok {t = (Tconst _ | Tvolatile _)} -> true
        | Tok {t = (Tstruct _ | Tunion _ | Tenum _ | Tclass _)} -> true
        | _ -> 
            aux xs
        )
  in

  xxs +> List.exists aux1 || aux xs

let look_like_only_idents xs =
  xs +> List.for_all (function
  | Tok {t=(TComma _ | TIdent _)} -> true
  (* when have cast *)
  | Parens _ -> true
  | _ -> false
  )


(* assumes a view without: 
 * - template arguments, qualifiers, 
 * - comments and cpp directives 
 * - TODO public/protected/... ?
 *)
let set_context_tag_cplus groups =
  let rec aux xs =
  match xs with
  | [] -> ()
  (* class Foo {, also valid for struct (and union, hmmm) *)
  | Tok{t=(Tclass _ | Tstruct _ | Tunion _);_}::Tok{t=TIdent(s,_);_}
    ::(Braces(t1, body, t2) as braces)::xs
    ->
      [braces] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InClassStruct s)::tok.TV.where;
      );
      aux (braces::xs)

  (* class Foo : ... { *)

  | Tok{t=Tclass _ | Tstruct _;_}::Tok{t=TIdent(s,_);_}
    ::Tok{t= TCol ii}::xs
    ->
      let (before, braces, after) =
        try 
          xs +> Common2.split_when (function
          | Braces _ -> true
          | _ -> false
          )
        with Not_found ->
          raise (UnclosedSymbol (spf "PB with split_when at %s"
                                    (Parse_info.string_of_info ii)))
      in
      aux before;
      [braces] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InClassStruct s)::tok.TV.where;
      );
      aux [braces];
      aux after

  (* TODO = {   InInitializer *)

  (* TODO <...> InTemplateParam *)

  (* TODO enum xxx { InEnum *)

  (* TODO xx(...) {  InFunction (can have some try or const or throw after 
   * the paren *)

  (* need to look what was before to help the look_like_xxx heuristics 
   *
   * The order of the 3 rules below is important. We must first try
   * look_like_argument which has less FP than look_like_parameter
  *)
  | x::(Parens(t1, body, t2) as parens)::xs 
    when look_like_argument x body ->
      msg_context t1.t (TV.InArgument);
      [parens] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InArgument)::tok.TV.where;
      );
      (* todo? recurse on body? *)
      aux [x];
      aux (parens::xs)

  (* special cases *)
  | (Tok{t=Toperator _} as tok1)::tok2::(Parens(t1, body, t2) as parens)::xs 
    when look_like_parameter tok1 body ->
      msg_context t1.t (TV.InParameter);
      [parens] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InParameter)::tok.TV.where;
      );
      (* recurse on body? hmm if InParameter should not have nested 
       * stuff except when pass function pointer 
       *)
      aux [tok1;tok2];
      aux (parens::xs)

  | x::(Parens(t1, body, t2) as parens)::xs 
    when look_like_parameter x body ->
      msg_context t1.t (TV.InParameter);
      [parens] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InParameter)::tok.TV.where;
      );
      (* recurse on body? hmm if InParameter should not have nested 
       * stuff except when pass function pointer 
       *)
      aux [x];
      aux (parens::xs)

  (* second tentative on InArgument, if xx(xx, yy, ww) where have only
   * identifiers, it's probably a constructed object!
   *)
  | Tok{t=TIdent _}::(Parens(t1, body, t2) as parens)::xs 
    when List.length body > 0 && look_like_only_idents body ->
      msg_context t1.t (TV.InArgument);
      [parens] +> TV.iter_token_multi (fun tok ->
        tok.TV.where <- (TV.InArgument)::tok.TV.where;
      );
      (* todo? recurse on body? *)
      aux (parens::xs)


  (* could be a cast too ... or what else? *)
  | x::(Parens(t1, body, t2) as parens)::xs ->
      (* let's default to something? hmm, no, got lots of regressions then 
       *  old: msg_context t1.t (TV.InArgument); ...
       *)
      aux [x];
      aux (parens::xs)
      

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


(*****************************************************************************)
(* Other *)
(*****************************************************************************)

(* 
 * TODO: right now this is less useful because we actually
 *  comment template args in a previous pass, but at some point this
 *  will be useful.
*)
let rec filter_for_typedef multi_groups = 

  (* a sentinel, which helps a few typedef heuristics which look
   * for a token before which would not work for the first toplevel
   * declaration.
   *)
  let multi_groups = 
    Tok(mk_token_fake (TPtVirg (Ast.fakeInfo())))::multi_groups in

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
        | Ttypedef _
        | Tunion _
          -> None

        | Tvirtual _ | Tfriend _ | Tinline _ | Tmutable _
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





(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

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

(* TODO 
 * TIdent_TemplatenameInQualifier ?
 *)

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

    (* need also to pass the top :: *)
    | ({t=TColCol _} as t2)::xs ->
        [t2] +> List.iter (fun tok ->
          change_tok tok 
            (TComment_Cpp (Token_cpp.CplusplusQualifier, TH.info_of_tok tok.t))
        );
        aux xs

    (* recurse *)
    | x::xs ->
        aux xs
  in
  aux xs


(* assumes a view where:
 * - set_context_tag has been called.
 * TODO: filter the 'explicit' keyword? filter the TCppDirectiveOther
 *  have a filter_for_constructor?
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
 * 
 * TODO: filter the TCppDirectiveOther,  have a filter_for_constructed?
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
        
    (* xx yy(1 ... *)
    | {t=TIdent_Typedef _;_}::{t=TIdent _;_}::
        ({t=TOPar (ii);where=InArgument::_;_} as tok1)::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs

    (* int yy(1 ... *)
    | {t=tok;_}::{t=TIdent _;_}::
      ({t=TOPar (ii);where=InArgument::_;_} as tok1)::xs 
      when TH.is_basic_type tok 
      ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs

    (* xx& yy(1 ... *)
    | {t=TIdent_Typedef _;_}::{t=TAnd _}::{t=TIdent _;_}::
        ({t=TOPar (ii);where=InArgument::_;_} as tok1)::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
          aux xs

    (* xx yy(zz)
     * The InArgument heuristic can't guess anything when just have
     * idents inside the parenthesis. It's probably a constructed
     * object though.
     * TODO? could be a function declaration, especially when at Toplevel.
     * If inside a function, then very probably a constructed object.
     *)
    | {t=TIdent_Typedef _;_}::{t=TIdent _;_}::
        ({t=TOPar (ii);} as tok1)::{t=TIdent _;_}::{t=TCPar _}::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs

    (* xx yy(zz, ww) *)
    | {t=TIdent_Typedef _;_}::{t=TIdent _;_}
      ::({t=TOPar (ii);} as tok1)
      ::{t=TIdent _;_}::{t=TComma _}::{t=TIdent _;_}
      ::{t=TCPar _}::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
          aux xs

    (* xx yy(&zz) *)
    | {t=TIdent_Typedef _;_}::{t=TIdent _;_}
      ::({t=TOPar (ii);} as tok1)
      ::{t=TAnd _}
      ::{t=TIdent _;_}
      ::{t=TCPar _}::xs ->

        change_tok tok1 (TOPar_CplusplusInit ii);
          aux xs

    (* int(), probably part of operator declaration 
     * could check that token before is a 'operator'
     *)
    | ({t=kind})::{t=TOPar _}::{t=TCPar _}::xs 
     when TH.is_basic_type kind ->
        aux xs

    (* int(...)  unless it's int( * xxx ) *)
    | ({t=kind})::{t=TOPar _}::{t=TMul _}::xs ->
      aux xs
    | ({t=kind} as tok1)::{t=TOPar _}::xs 
     when TH.is_basic_type kind ->
        let newone = 
          match kind with
          | Tchar ii -> Tchar_Constr ii
          | Tshort ii -> Tshort_Constr ii
          | Tint ii -> Tint_Constr ii
          | Tdouble ii -> Tdouble_Constr ii
          | Tfloat ii -> Tfloat_Constr ii
          | Tlong ii -> Tlong_Constr ii
          | Tbool ii -> Tbool_Constr ii
          | Tunsigned ii -> Tunsigned_Constr ii
          | Tsigned ii -> Tsigned_Constr ii
          | _ -> raise Impossible
        in
        change_tok tok1 newone;
        aux xs

    (* recurse *)
    | x::xs -> aux xs
  in
  aux xs
