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

module E = Error_php

module Flag = Flag_analyze_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Checking the use of class variables, class constants, and class names.
 * 
 * For methods most of the checks are actually done in check_functions_php.ml
 * as the logic for arity method checking is similar to function arity
 * checking.
 * 
 * todo: 
 *  - check on static class vars, 
 *  - check on constants, 
 *  - check on fields
 *  - check in strict mode that calls a static method with a qualifier,
 *    not with $this-> ... ugly
 * 
 * Note that many checks on methods are actually done in 
 * check_functions_php.ml .
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Typing rules *)
(*****************************************************************************)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

let visit_and_check_new  find_entity prog =
  let visitor = V.mk_visitor { Visitor_php.default_visitor with
    Visitor_php.kexpr = (fun (k,vx) x ->
      match Ast_php.untype x with
      | New (tok, (ClassNameRefStatic (ClassName class_name)), args) ->

          (* todo: use lookup_method *)
          E.find_entity_and_warn find_entity (Entity_php.Class, class_name)
          (function Ast_php.ClassE def ->
            (*
              Check_functions_php.check_args_vs_params 
              (callname,   args +> Ast.unparen +> Ast.uncomma)
              (def.f_name, def.f_params +> Ast.unparen +> Ast.uncomma)
            *)
            ()
          | _ -> raise Impossible
          );
          k x

      | New (tok, (ClassNameRefStatic (Self _ | Parent _)), args) ->
          failwith "check_functions_php: call unsugar_self_parent()"
      | New (tok, (ClassNameRefDynamic (class_name, _)), args) ->
          (* can't do much *)
          k x
      | _ -> k x
    );
  } in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_program2 find_entity prog = 
  visit_and_check_new find_entity prog

let check_program a b = 
  Common.profile_code "Checker.classes" (fun () -> 
    check_program2 a b)
