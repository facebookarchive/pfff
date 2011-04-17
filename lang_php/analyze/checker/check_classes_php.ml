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

(* todo: see defs_uses_php.ml now *)

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

(* some of those checks are also done in check_variables_php.ml which
 * needs also to access class information to knows which member variables
 * are ok
 *)
let visit_and_check_new_and_extends  ?(find_entity = None) prog =
  let visitor = V.mk_visitor { Visitor_php.default_visitor with
    Visitor_php.kexpr = (fun (k,vx) x ->
      match Ast_php.untype  x with
      | New (tok, (ClassNameRefStatic (ClassName class_name)), args) ->

          E.find_entity ~find_entity (Entity_php.Class, class_name)
          +> Common.do_option (fun id_ast ->
            match id_ast with
            | Ast_php.ClassE def ->
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
          pr2 "TODO: handling ClassNameRefStatic of self or parent";
          k x
      | New (tok, (ClassNameRefDynamic class_name), args) ->
          pr2 "TODO: handling ClassNameRefDynamic";
          k x
      | _ -> k x
    );
  } in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check_program2 ?find_entity prog = 
  visit_and_check_new_and_extends ?find_entity prog


let check_program ?find_entity a = 
  Common.profile_code "Checker.classes" (fun () -> 
    check_program2 ?find_entity a)
