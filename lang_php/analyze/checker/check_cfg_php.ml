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

module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * TODO: check dead statements for toplevel blocks ?
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_program2 ?find_entity prog =
  let visitor = V.mk_visitor { V.default_visitor with
    V.kfunc_def = (fun (k, _) def ->
      try
          let flow = Controlflow_build_php.cfg_of_func def in
          Controlflow_build_php.deadcode_detection flow;
          ()
       with Controlflow_build_php.Error err ->
         E.fatal (E.CfgError err);
    );
    V.kmethod_def = (fun (k, _) def ->
      match def.m_body with
      (* do not go in abstract method, because cfg_of_method generate an
       * exn in such case
       *)
      | AbstractMethod _ -> ()
      | MethodBody _ ->
          try
            let flow = Controlflow_build_php.cfg_of_method def in
            Controlflow_build_php.deadcode_detection flow;
          with Controlflow_build_php.Error err ->
            E.fatal (E.CfgError err);
    );
  }
  in
  visitor (Program prog)

let check_program ?find_entity a = 
  Common.profile_code "Checker.cfg" (fun () -> check_program2 ?find_entity a)
