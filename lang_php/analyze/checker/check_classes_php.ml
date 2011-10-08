(* Yoann Padioleau
 *
 * Copyright (C) 2010-2011 Facebook
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
module Ent = Entity_php
module Flag = Flag_analyze_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Checking the use of method calls, class variables, class constants, 
 * and class names.
 * 
 * todo: 
 *  - check on static class vars, 
 *  - check on constants, 
 *  - check on fields
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type context_call =
  | StaticCall
  | MethodCall of bool (* abstract class ? *)

let check_method_call context (aclass, amethod) (name, args) find_entity =
  let loc = Ast.info_of_name name in
  try
    let def =
      Class_php.lookup_method 
        (* todo: remove at some point, but too many errors for now *)
        ~case_insensitive:true
        (aclass, amethod) find_entity in
    if Check_functions_php.contain_func_name_args_like (ClassStmt (Method def))
    then pr2_once ("not checking calls to code using func_num_args()-like")
    else begin
      (* check if used in the right way ... *)
      (match context, Class_php.is_static_method def with
      | StaticCall, true -> ()
      | MethodCall _, false -> ()
      | StaticCall, false ->
          if amethod =$= Class_php.constructor_name
          then ()
          else E.fatal loc (E.CallingMethodWithQualifier amethod)
      | MethodCall _, true ->
          E.fatal loc (E.CallingStaticMethodWithoutQualifier amethod)
      );
      Check_functions_php.check_args_vs_params 
        (name, args +> Ast.unparen +> Ast.uncomma)
        (def.m_name, def.m_params+>Ast.unparen+>Ast.uncomma_dots)
    end
  with
  | Class_php.Use__Call ->
      (* not much we can do then, let's bailout *)
      ()
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal loc (E.UndefinedClassWhileLookup (s))
  | Not_found ->
      (match context with
      | StaticCall -> 
          E.fatal loc (E.UndefinedEntity (Ent.StaticMethod, amethod))
      | MethodCall false -> 
          E.fatal loc (E.UndefinedEntity (Ent.Method, amethod))
      | MethodCall true -> 
          E.fatal loc (E.UndefinedMethodInAbstractClass amethod)
      )

  | Multi_found -> 
      (* this can happen with multiple classes with same name, not our job *)
      ()

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(* pre: have a unsugar AST regarding self/parent *)
let visit_and_check  find_entity prog =

  (* less: similar to what we do in unsugar_self_parent, do "$this"
   * unsugaring there too?
   *)
  let in_class = ref (None: (string * bool) option) in

  let visitor = V.mk_visitor { Visitor_php.default_visitor with

    V.kclass_def = (fun (k, _) def ->
      let is_abstract = 
        match def.c_type with
        | ClassAbstract _ -> true
        | _ -> false
      in
      Common.save_excursion in_class (Some (Ast.name def.c_name, is_abstract)) 
        (fun () ->
          k def
        )
    );
    V.klvalue = (fun (k,vx) x ->
      match Ast.untype x with
      | StaticMethodCallSimple (qu, name, args) ->
          (match fst qu with
          | ClassName (classname) ->
              let aclass = Ast.name classname in
              let amethod = Ast.name name in
              check_method_call StaticCall
                (aclass, amethod) (name, args) find_entity

          | (Self _ | Parent _) ->
              failwith "check_functions_php: call unsugar_self_parent()"
          | LateStatic _ ->
              (* not much we can do? *)
              ()
          );
          k x

      | MethodCallSimple (lval, _tok, name, args) ->
          (* if one calls a method via $this, then it's quite easy to check
           * the arity (eletuchy's idea?).
           * Being complete and handling any method calls like $o->foo()
           * requires to know what is the type of $o which is quite
           * complicated ... so let's skip that for now.
           * 
           * todo: special case also id(new ...)-> ?
           *)
          (match Ast.untype lval with
          | This _ ->
              (match !in_class with
              | Some (aclass, is_abstract) ->
                  let amethod = Ast.name name in
                  check_method_call (MethodCall is_abstract)
                    (aclass, amethod) (name, args) find_entity
              | None ->
                  (* wtf? use of $this outside class ??? *)
                  ()
              )
          | _ -> 
              (* todo: need dataflow ... *)
              ()
          );
          k x
               
      | FunCallVar _ -> 
          (* not much we can do there too ... *)
          k x

      | _ -> k x
    );

    V.kexpr = (fun (k,vx) x ->
      match Ast_php.untype x with
      | New (tok, (ClassNameRefStatic (ClassName class_name)), args) ->

          (* todo: use lookup_method *)
          E.find_entity_and_warn find_entity (Entity_php.Class, class_name)
          (function Ast_php.ClassE def ->
            (*
              Check_functions_php.check_args_vs_params 
              (callname,   args +> Ast.unparen +> Ast.uncomma)
              (def.f_name, def.f_params +> Ast.unparen +> Ast.uncomma)
                    TODO: too many FP for now
                       if !Flag.show_analyze_error
                       then pr2_once (spf "Could not find constructor for: %s" 
                                         (Ast.name name));
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
let check_program a b = 
  Common.profile_code "Checker.classes" (fun () -> visit_and_check a b)
