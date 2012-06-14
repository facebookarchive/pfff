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
module Ent = Database_code
module Flag = Flag_analyze_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Checking the use of method calls, member fields, 
 * TODO class variables, TODO class constants, SEMI and class names.
 * TODO check interface, traits.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type context_call =
  | StaticCall
  | MethodCall of bool (* abstract class ? *)

type context_access =
  | StaticAccess
  | ObjAccess

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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
  (* not much we can do then, let's bailout? *)
  | Class_php.Use__Call ->
      pr2_once "not analyzing code using __call"
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal loc (E.UndefinedClassWhileLookup (s))
  | Not_found ->
      (match context with
      | StaticCall -> 
          E.fatal loc (E.UndefinedEntity (Ent.Method Ent.StaticMethod,amethod))
      | MethodCall false -> 
          E.fatal loc (E.UndefinedEntity (Ent.Method Ent.RegularMethod,amethod))
      | MethodCall true -> 
          E.fatal loc (E.UndefinedMethodInAbstractClass amethod)
      )
  (* this can happen with multiple classes with same name, not our job *)
  | Multi_found -> ()

let check_member_access ctx (aclass, afield) loc find_entity =
  try 
    let _ = 
      Class_php.lookup_member
        (* todo: remove at some point, but too many errors for now *)
        ~case_insensitive:true
        (aclass, afield) find_entity
    in
    (* todo: check if used in the right way ... *)
    ()
  with
  (* todo: Use__Get and other magic shit?  *)
  | Not_found -> 
      (match ctx with
      | StaticAccess ->
          E.fatal loc (E.UndefinedEntity (Ent.Field, afield))
      | ObjAccess ->
          let allmembers = Class_php.collect_members aclass find_entity 
            +> List.map Ast.dname
          in
          (* todo? could also show a strong warning when the list
           * allmembers is big, in which case if most of the
           * fields were defined, it makes sense to force people to 
           * define them all.
           *)
          let suggest = Suggest_fix_php.suggest afield allmembers in
          E.fatal loc (E.UseOfUndefinedMember (afield, suggest))
      )
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal loc (E.UndefinedClassWhileLookup s)
  | Multi_found -> ()

let check_class_constant (aclass, s) tok find_entity =
  try 
    let _ = Class_php.lookup_constant (aclass, s) find_entity in
    ()
  with 
  | Not_found -> 
      E.fatal tok (E.UndefinedEntity (Ent.ClassConstant, s))
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal tok (E.UndefinedClassWhileLookup s)
  | Multi_found -> ()


(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(* pre: have a unsugar AST regarding self/parent *)
let visit_and_check  find_entity prog =

  (* less: similar to what we do in unsugar_self_parent, do "$this"
   * unsugaring there too?
   *)
  let in_class = ref (None: (string * bool) option) in
  let in_trait = ref false in

  let visitor = V.mk_visitor { Visitor_php.default_visitor with

    V.kclass_def = (fun (k, _) def ->
      let is_abstract = 
        match def.c_type with ClassAbstract _ -> true | _ -> false in
      let is_trait =
        match def.c_type with Trait _ -> true | _ -> false in

      def.c_extends +> Common.do_option (fun (tok, parent) ->
        E.find_entity_and_warn find_entity (Ent.Class Ent.RegularClass, parent)
          (fun _ ->
          ()
        )
      );

      Common.save_excursion in_class (Some (Ast.name def.c_name, is_abstract))
      (fun () ->
      Common.save_excursion in_trait is_trait
      (fun () ->
          k def
        ))
    );
    V.klvalue = (fun (k,vx) x ->
      match x with
      | StaticMethodCallSimple (qu, name, args) ->
          (match fst qu with
          | ClassName (classname) ->
              let aclass = Ast.name classname in
              let amethod = Ast.name name in
              check_method_call StaticCall
                (aclass, amethod) (name, args) find_entity

          | (Self _ | Parent _) ->
              if !in_trait
              (* checking for right method name should be done at use time, it
               * can't be done here, so let's accept any method call here.
               *)
              then ()
              else failwith "check_classes_php: call unsugar_self_parent()"
          (* not much we can do? *)
          | LateStatic _ -> ()
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
          (match lval, !in_class with
          | This _, Some (aclass, is_abstract) ->
              let amethod = Ast.name name in
              check_method_call (MethodCall is_abstract)
                (aclass, amethod) (name, args) find_entity
          (* wtf? use of $this outside class ??? *)
          | _, None -> ()
          (* todo: need dataflow ... *)
          | _, _ -> ()
          );
          k x
               
      | FunCallVar _ -> 
          (* not much we can do there too ... *)
          k x

      | ClassVar ((ClassName classname, tok), dname) ->
          check_member_access StaticAccess 
            (Ast.name classname, Ast.dname dname) tok find_entity

      | ObjAccessSimple (lval, tok, name) ->
          let field = Ast.name name in
          (match lval, !in_class with
          | This _, Some (aclass, is_abstract) ->
              check_member_access ObjAccess (aclass, field) tok find_entity
          (* todo: need dataflow ... *)
          | _, _ -> ()
          )

      | _ -> k x
    );

    V.kexpr = (fun (k,vx) x ->
      match x with
      | New (tok, (ClassNameRefStatic (ClassName class_name)), args) ->

          (* todo: use lookup_method *)
          E.find_entity_and_warn find_entity (Ent.Class Ent.RegularClass, 
                                             class_name)
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
          failwith "check_classes_php: call unsugar_self_parent()"
      | New (tok, (ClassNameRefDynamic (class_name, _)), args) ->
          (* can't do much *)
          k x
      | _ -> k x
    );
    V.kscalar = (fun (k, _) x ->
      (match x with
      | ClassConstant ((ClassName classname, tok), name) ->
          check_class_constant (Ast.name classname, Ast.name name) tok
            find_entity
      | _ -> ()
      );
      k x
    );
    
  } in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let check_program a b = 
  Common.profile_code "Checker.classes" (fun () -> visit_and_check a b)
