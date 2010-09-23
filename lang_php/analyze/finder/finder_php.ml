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

module Ast = Ast_php
module V = Visitor_php 

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* was in matcher/ and lib_matcher/ before, but cleaner put all yacfe
 * code together.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)


(*****************************************************************************)
(* Specific finder wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Position based queries *)
(*****************************************************************************)

(* used in gui *)
let (info_at_pos: int -> Ast.toplevel -> Ast.info) = fun pos top -> 

  let res = ref [] in
  let hooks = { V.default_visitor with
    V.kinfo = (fun (k, bigf) ii -> 

        let startoffset =
          Ast.pos_of_info ii in
        let endoffset =
          startoffset + String.length (Ast.str_of_info ii)
        in
        if Ast.is_origintok ii &&
            pos >= startoffset &&
            pos <= endoffset
        then Common.push2 ii res;
    );
  } in
  let visitor = V.mk_visitor hooks in
  visitor.V.vtop top;
  Common.list_to_single_or_exn !res


let (info_at_pos_in_full_program: int -> Ast.program -> Ast.info) = 
 fun pos asts -> 

   let res = ref [] in

   asts +> List.iter (fun ast -> 
      try 
        let info = info_at_pos pos ast in
        Common.push2 info res;
      with
      | Not_found -> 
          ()
      | Multi_found -> 
          raise Multi_found
   );
   Common.list_to_single_or_exn !res



(* used in GUI *)
let (expr_at_pos: int -> Ast.toplevel -> Ast.expr) = fun pos top -> 

  let res = ref [] in

  let found_in_nested = ref false in

  let hooks = { V.default_visitor with

    V.kexpr = (fun (k, bigf) e -> 

      (* recurse *)
      k e;
      if not !found_in_nested then begin

        let ii = Lib_parsing_php.ii_of_expr e in
        let (min, max) = Lib_parsing_php.min_max_ii_by_pos ii in
        
        let startoffset = Ast_php.pos_of_info min in
        let endoffset = 
          Ast_php.pos_of_info max + 
            String.length (Ast_php.str_of_info max) in

        if pos > startoffset && pos <= endoffset
        then begin
          found_in_nested := true;
          Common.push2 e res;
        end
      end
    );
  }
  in
  let visitor = V.mk_visitor hooks in
  visitor.V.vtop top;
  Common.list_to_single_or_exn !res


(*
let (expr_at_pos_in_full_program: int -> Ast.program -> Ast.expression) = 
 fun pos asts -> 

   let res = ref [] in
   asts +> List.iter (fun ast -> 
      try 
        let info = expr_at_pos pos ast in
        Common.push2 info res;
      with
      | Not_found -> 
          ()
      | Multi_found -> 
          raise Multi_found
   );
   Common.list_to_single_or_exn !res

*)
