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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TODO 
 * TIdent_TemplatenameInQualifier 
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let set_as_opar_cplusplus xs = 
  match xs with
  | ({t=TOPar ii;_} as tok1)::xs -> 
      change_tok tok1 (TOPar_CplusplusInit ii);
  | _ -> raise  Impossible

let templateLOOKAHEAD = 30
  
(* note: no need to check for TCPar to stop for instance the search, 
 * this is will be done automatically because we would be inside a 
 * Parenthised expression.
 *)
let rec have_a_tsup_quite_close xs =
  match xs with
  | [] -> false
  | (PToken x)::xs -> 
      (match x with
      | {t=TSup i} -> true

      (* false positive *)
      | {t=tok} when TH.is_static_cast_like tok -> false

      (* bugfix: but want allow some binary operator :) like '*' *)
      | {t=tok} when TH.is_binary_operator_except_star tok -> false
      

      | x -> have_a_tsup_quite_close xs
      )
  | (Parenthised (xxs, info_parens))::xs -> 
      have_a_tsup_quite_close xs

(* precondition: there is a tsup *)
let rec find_tsup_quite_close xs = 
  let rec aux acc xs =
    match xs with
    | [] -> failwith "PB: find_tsup_quite_close, no tsup"
    | (PToken x)::xs -> 
        pr2_gen x;
        (match x with
        | {t=TSup ii} -> 
            acc, (x,ii), xs
              
        | {t=TInf ii} -> 
            (* recurse *)
            let (before, (tsuptok,_), after) = 
              find_tsup_quite_close xs in
            (* we don't care about this one, it will be eventually be 
             * transformed by the caller *)
            aux ((PToken x)::(before++[PToken tsuptok])) xs
              
              
        | x -> aux ((PToken x)::acc) xs
        )
    | (Parenthised (xxs, info_parens))::xs -> 
        aux ((Parenthised (xxs, info_parens))::acc) xs
  in
  let (before, tsup, after) = aux [] xs in
  List.rev before, tsup, after

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)

let find_view_all_tokens xs = 
 let rec aux xs =
  match xs with
  | [] -> ()
  | x::xs -> aux xs
 in
 aux xs

let find_view_filtered_tokens xs = 
 let rec aux xs =
  match xs with
  | [] -> ()

  (* classname *)
  | ({t=TIdent(s,i1)} as tok1)
    ::{t=TColCol _}
    ::({t=TIdent(s2,i2)} as tok2)
    ::xs -> 
      change_tok tok1 (TIdent_ClassnameInQualifier (s,i1));
      if s = s2 
      then change_tok tok2 (TIdent_Constructor (s2, i2));

      aux (tok2::xs)

  | ({t=TIdent(s,i1)} as tok1)
    ::{t=TColCol _}
    ::({t=Toperator(i2)} as tok2)
    ::xs -> 
      change_tok tok1 (TIdent_ClassnameInQualifier (s, i1));
      aux (tok2::xs)

   (* destructor special case *)
  | ({t=TIdent(s,i1)} as tok1)
    ::{t=TColCol _}
    ::{t=TTilde _}
    ::({t=TIdent(s2,i2)} as _tok2)
    ::xs -> 
      change_tok tok1 (TIdent_ClassnameInQualifier (s, i1));
      aux xs

  (* operator special case to avoid ambiguity when have 
   * x.operator new[3], which apparently is possible with 
   * the grammar, so to know diff between new[] operator and 
   * new [3] array access need see 2 tokens ahead, so again,
   * introduce an extra token to help.
   *)
  | ({t=Tnew i1} | {t=Tdelete i1})
      ::({t=TOCro i2} as tok2)
      ::({t=TCCro i3} as tok3)
      ::xs -> 
      change_tok tok2 (TOCro_new i2);
      change_tok tok3 (TCCro_new i3);

      aux xs

  (* recurse *)
  | x::xs -> aux xs
 in
 aux xs

(* assume have done TInf -> TInf_Template, TIdent_ClassnameInQualifier, 
 * and TIdent_Templatename.
 * Pass 2!!
 *)

let find_view_filtered_tokens_bis xs = 
 let rec aux xs =
  match xs with
  | [] -> ()

  (* xx::yy<zz> ww *)
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=(*TypedefIdent*) _})
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::(({t=TIdent (s2,_)})| ({t=TIdent_ClassnameInQualifier (s2,_)}))
    ::xs

  (* xx::yy<zz>& ww *)
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=(*TypedefIdent*) _})
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::({t=TAnd _})
    ::(({t=TIdent (s2,_)})| ({t=TIdent_ClassnameInQualifier (s2,_)}))
    ::xs

  (* xx::yy<zz*> ww *)
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=(*TypedefIdent*) _})
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TMul _})
    ::({t=TSup_Template _})
    ::(({t=TIdent (s2,_)})| ({t=TIdent_ClassnameInQualifier (s2,_)}))
    ::xs
  (* xx::yy<zz*>& ww *)
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=(*TypedefIdent*) _})
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TMul _})
    ::({t=TSup_Template _})
    ::({t=TAnd _})
    ::(({t=TIdent (s2,_)})| ({t=TIdent_ClassnameInQualifier (s2,_)}))
    ::xs
    -> 
      change_tok tok1 (TIdent_ClassnameInQualifier_BeforeTypedef (s1, i1));
      change_tok tok2 (TColCol_BeforeTypedef i2);
      
      aux xs

  (* xx<zz>::yy ww *)
  | ({t=TIdent_Templatename (s1,i1)} as tok1)
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::({t=TColCol i2} as tok2)
    ::({t=TIdent (s2,_)})
    ::xs

  (* xx<zz,zzz>::yy ww *)
  | ({t=TIdent_Templatename (s1,i1)} as tok1)
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::({t=TColCol i2} as tok2)
    ::({t=TIdent (s2,_)})
    ::xs

  (* xx<zz*>::yy ww *)
  | ({t=TIdent_Templatename (s1,i1)} as tok1)
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*)_})
    ::({t=TMul _})
    ::({t=TSup_Template _})
    ::({t=TColCol i2} as tok2)
    ::({t=TIdent (s2,_)})
    ::xs
    -> 
      change_tok tok1 (TIdent_TemplatenameInQualifier_BeforeTypedef (s1, i1));
      change_tok tok2 (TColCol_BeforeTypedef i2);
      
      aux xs

  (* aa::xx<zz>::yy ww *)
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=TIdent_Templatename (s3,i3)} as tok3)
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::({t=TColCol i4} as tok4)
    ::({t=TIdent (s2,_)})
    ::xs 
  | ({t=TIdent_ClassnameInQualifier (s1,i1)} as tok1)
    ::({t=TColCol i2} as tok2)
    ::({t=TIdent_Templatename (s3,i3)} as tok3)
    ::({t=TInf_Template _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=(*TIdent (s,_)*) _})
    ::({t=TSup_Template _})
    ::({t=TColCol i4} as tok4)
    ::({t=TIdent (s2,_)})
    ::xs 
    -> 
      change_tok tok1 (TIdent_ClassnameInQualifier_BeforeTypedef (s1, i1));
      change_tok tok2 (TColCol_BeforeTypedef i2);
      change_tok tok3 (TIdent_TemplatenameInQualifier_BeforeTypedef (s3, i3));
      change_tok tok4 (TColCol_BeforeTypedef i4);
      
      aux xs

  (* recurse *)
  | x::xs -> aux xs
 in
 aux xs


let find_view_parenthized xs = 
 let rec aux xs =
  match xs with
  | [] -> ()
      
  (* static_cast *)
  | (PToken {t=tok1})::(PToken ({t=TInf i2} as tok2))::xs 
    when TH.is_static_cast_like tok1 -> 
      change_tok tok2 (TInf_Template i2);

      let (before_inf, (toksup, toksupi), rest) = find_tsup_quite_close xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_inf;
      aux rest

  (* templatename *)

  (* note: some macros in standard.h may expand to static_cast, so perhaps
   * better to do template detection after macro expansion ?
   *)
 
  (* Template, maybe. Rely on fact that even if C++ allow expression like
   * a<b>c, parsed as (a < b) > c, most people don't use that, so we
   * can use this fact for our heuristic. Also people usually don't
   * nest < for expressions as in a < b < c.
   *)
  | (PToken ({t=TIdent(s,i1)} as tok1))
    ::(PToken ({t=TInf i2} as tok2))::xs 
    when have_a_tsup_quite_close (Common.take_safe templateLOOKAHEAD xs)
    -> 
      change_tok tok1 (TIdent_Templatename(s,i1));
      change_tok tok2 (TInf_Template i2);
      let (before_inf, (toksup, toksupi), rest) = find_tsup_quite_close xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_inf;
      aux rest

  | (PToken ({t=Ttemplate(i1)}))
    ::(PToken ({t=TInf i2} as tok2))::xs 
   when have_a_tsup_quite_close (Common.take_safe templateLOOKAHEAD xs) ->
      change_tok tok2 (TInf_Template i2);
      
      let (before_inf, (toksup, toksupi), rest) = find_tsup_quite_close xs in
      change_tok toksup (TSup_Template toksupi);
      
      (* recurse *)
      aux before_inf;
      aux rest

        
  (* new (...) type(arg) *)
  | (PToken ({t=Tnew(i1)}))
    ::Parenthised _
    ::PToken ({t=TIdent(s,i2)} as tok1)
    ::Parenthised _
    ::xs 
    ->
      change_tok tok1 (TIdent_Typedef(s,i2));
      aux xs


  (* recurse *)
  | (PToken x)::xs -> aux xs 
  | (Parenthised (xxs, info_parens))::xs -> 
      xxs +> List.iter aux;
      aux xs
 in
 aux xs

let find_view_parenthized2 xs = 
 let rec aux xs =
  match xs with
  | [] -> ()

  (* recurse *)
  | (PToken x)::xs -> aux xs 
  | (Parenthised (xxs, info_parens))::xs -> 
      xxs +> List.iter aux;
      aux xs
 in
 aux xs
      

(* pre: have done the templatenameize and TInf/TSup -> TInf2/TSup2 *)
let rec find_view_line_paren xs = 

 let rec aux xs =
  match xs with 
  | [] -> ()

  (* c++ext: constructed objects part1 *)

  (* xx yy(...);    when in body of function/method, otherwise can
   * perfectly be a declaration of a prototype or of a method.
   *)
  | (Line 
        [PToken ({t=TIdent (s1,i1)} as tok);
         PToken ({t=TIdent (s,_); where = ctx} as _constructor);
         Parenthised (xxs,info_parens);
         PToken ({t=TPtVirg _});
        ]
    )
    ::xs 
    when List.hd ctx = InFunction -> 
      ignore(info_parens);
      (* todo? check that in info_parens have what looks like expressions *)

      change_tok tok (TIdent_Typedef (s1,i1));
      set_as_opar_cplusplus info_parens;

      aux xs

  (* xx<zz> yy(...); when in function *)
  | (Line 
        [PToken ({t=TIdent_Templatename (s1,i1)} as _tok);
         PToken ({t=TInf_Template _});
         PToken ({t=(*TypedefIdent*) _ident});
         PToken ({t=TSup_Template _});
         PToken ({t=TIdent (s,_); where = ctx} as _constructor);
         Parenthised (xxs,info_parens);
         PToken ({t=TPtVirg _});
        ]
    )
    ::xs 
    when List.hd ctx = InFunction -> 
      ignore(info_parens);
      (* todo? check that in info_parens have what looks like expressions *)

      set_as_opar_cplusplus info_parens;

      aux xs


  (* xx<zz> yy \n(...); when in function *)
  | (Line 
        [PToken ({t=TIdent_Templatename (s1,i1)} as _tok);
         PToken ({t=TInf_Template _});
         PToken ({t=(*TypedefIdent*) _});
         PToken ({t=TSup_Template _});
         PToken ({t=TIdent (s,_); where = ctx} as _constructor);
        ]
    )
    ::(Line 
         [
         Parenthised (xxs,info_parens);
         PToken ({t=TPtVirg _});
        ]
    )
    ::xs 
    when List.hd ctx = InFunction -> 
      ignore(info_parens);
      (* todo? check that in info_parens have what looks like expressions *)

      set_as_opar_cplusplus info_parens;

      aux xs

  | x::xs -> 
      aux xs
 in
 aux xs
