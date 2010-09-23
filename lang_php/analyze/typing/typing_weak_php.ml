(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_php

module Flag = Flag_analyze_php
module Ast = Ast_php

module V = Visitor_php
module T = Type_php
module N = Namespace_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Type inference is more complicated in the presence of:
 * - records, as the equational tricks used by the unification algorithm
 *    do not work anymore (need to use row type variables; 
 *    Didier Remy's trick)
 * - union types, which are required by the PHP library and which are
 *   maybe needed for some flexibility for certain PHP programmers.
 *   Those union types again do not allow to use the equational tricks.
 * 
 * 
 * As type inference is complicated, we can provide a weaker version
 * of type inference by doing some simpler analysis, such as wether
 * the variable is used in a null context, int context, or wether a
 * certain field is used with this variable. This is the goal of
 * this module. Once the type inference algorithm will work
 * (typing_php.ml), then this module will become obsolete. 
 * 
 * One good thing is that this weaker analysis also requires 
 * an interprocedural analysis and possibly information from builtin 
 * functions, which both are orthogonals I think to the type inference
 * problem. So we can make progress on those 2 easier fronts while
 * still improving on the side the type inference algorithm.
 * 
 * Note that this module will not be able to deduce certain information,
 * for instance when do some aliases, when accessed the same thing
 * through a record access. Also the kind of record info it will infer
 * are just of depth 1.
 * 
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_typing

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let extract_fields_per_var top =
  let hvars = Hashtbl.create 101 in

  let hooks = { V.default_visitor with 
    V.klvalue = (fun (k,vx) v ->
      match untype v with
      | VArrayAccess (var2, expr_bracket) ->
          (match untype var2, Ast.unbracket expr_bracket with
          | (Var (dname, scope), 
            Some (Sc (C (Ast.String (s, info))), t)) ->
              
              let svar = N.dnameS_of_dname dname in
              Common.hupdate_default svar 
                (fun oldh -> 
                  Hashtbl.replace oldh s true;
                  oldh
                ) (fun () -> Hashtbl.create 11) hvars;
              k v
          | _ -> k v
          )
      | _ -> k v
    );
  }
  in
  (V.mk_visitor hooks).V.vtop top;
  hvars +> Common.hash_to_list +> List.map (fun (s,h) -> 
    s, Common.hashset_to_list h
  )


let info_of_func top = 
  raise Todo

