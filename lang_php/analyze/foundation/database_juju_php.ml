(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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

open Ast_php_simple
module A = Ast_php_simple
module Env = Env_interpreter_php
module SMap = Map.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Yet another "code database" for php functions/classes/constants.
 * This one is used by the abstract interpreter.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type database = {
  funs_juju    : Ast_php_simple.func_def Common.cached SMap.t ref;
  classes_juju : Ast_php_simple.class_def Common.cached SMap.t ref;
  constants_juju: Ast_php_simple.constant_def Common.cached SMap.t ref;
}

(*****************************************************************************)
(* Code database *)
(*****************************************************************************)

(* todo: position_info flag *)
let juju_db_of_files ?(show_progress=false) xs =
  let db = {
    funs_juju = ref SMap.empty;
    classes_juju = ref SMap.empty;
    constants_juju = ref SMap.empty;
  }
  in
  xs +> Common_extra.progress ~show:show_progress (fun k -> 
   List.iter (fun file ->
    k();
    try
      let cst = Parse_php.parse_program file in		
      let ast = Ast_php_simple_build.program cst in
      List.iter (fun x ->
        (* todo: print warning when duplicate class/func ? *)
        match x with
        | ClassDef c ->
            db.classes_juju := 
              SMap.add (A.unwrap c.c_name) (Common.serial c) !(db.classes_juju)
        | FuncDef fd ->
            db.funs_juju := 
              SMap.add (A.unwrap fd.f_name) (Common.serial fd) !(db.funs_juju)
        | ConstantDef c ->
            db.constants_juju :=
              SMap.add (A.unwrap c.cst_name) (Common.serial c) !(db.constants_juju)

        | (Global _|StaticVars _
          |Try (_, _, _)|Throw _
          |Continue _|Break _|Return _
          |Foreach (_, _, _, _)|For (_, _, _, _)|Do (_, _)|While (_, _)
          |Switch (_, _)|If (_, _, _)
          |Block _|Expr _
          ) -> ()
      ) ast
    with e -> 
      Common.pr2 (spf "ERROR in %s, exn = %s" file (Common.exn_to_s e))
  ));
  db

(* todo: what if multiple matches?
 * todo: profiling information
 *)
let code_database_of_juju_db db = { Env.
  funs      = (fun s -> let f = SMap.find s !(db.funs_juju) in 
                        Common.unserial f);
  classes   = (fun s -> let c = SMap.find s !(db.classes_juju) in 
                        Common.unserial c);
  constants = (fun s -> let c = SMap.find s !(db.constants_juju) in 
                        Common.unserial c);
  }
