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

module Ent = Entity_php

module Flag = Flag_analyze_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * history: repeat what iproctor wanted for his Strict mode that I first
 * coded in hphpi
 * 
 * related work:
 *  - miyamide
 *  - PHP-sat at http://strategoxt.org/PHP/PhpSat
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Arity check helpers *)
(*****************************************************************************)
(* cf also type_php.ml *)

let no_check_when_contain = [
  "func_num_args";
  "func_get_args";
]

let contain_func_name_args_like any =
  let funcalls = Lib_parsing_php.get_funcalls_any any in
  no_check_when_contain +> List.exists (fun danger_func -> 
    List.mem danger_func funcalls
  )


let check_args_vs_params (callname, all_args) (defname, all_params) =

  let info = Ast_php.info_of_name callname in
  let str_def = Ast.name defname in

  let rec aux args params = 
    match args, params with
    | [], [] -> ()
    | [], y::ys ->
        if y.p_default = None 
        then E.fatal info (E.NotEnoughArguments str_def)
        else aux [] ys
    | x::xs, [] ->
        E.fatal info (E.TooManyArguments str_def)
    | x::xs, y::ys ->
        (match x with
        (* erling's idea of wrong keyword argument check *)
        | Arg(Assign((Var(dn, _), _),_ , expr), _) ->
            if not (Ast.dname dn =$= Ast.dname y.p_name)
            then
              
              let all_params_str = 
                all_params +> List.map (fun p -> Ast.dname p.p_name) in
              let severity =
                if List.mem (Ast.dname dn) all_params_str
                then E.ReallyReallyBad
                else 
                  (* todo: edit_distance *)
                  E.Bad
              in
              let loc = Ast.info_of_dname dn in
              let s = Ast.dname dn in
              let param = Ast.dname y.p_name in
              E.fatal loc (E.WrongKeywordArgument(s, param, severity))
        | _ -> ()
        );
        aux xs ys
  in
  aux all_args all_params

let check_method_call (aclass, amethod) (name, args) find_entity =
  try 
    let def =
      Class_php.lookup_method (aclass, amethod) find_entity in
    let contain_func_num_args = 
      contain_func_name_args_like (ClassStmt (Method def)) in
    
    if contain_func_num_args
    then pr2_once ("not checking calls to code using " ^ 
                      "func_num_args() or alike")
    else 
      check_args_vs_params 
        (name, args +> Ast.unparen +> Ast.uncomma)
        (def.m_name, def.m_params+>Ast.unparen+>Ast.uncomma_dots)
  with
  (* could also be reported elsewhere too *)
  | Not_found ->
      let loc = Ast.info_of_name name in
      E.fatal loc (E.UndefinedEntity (Ent.StaticMethod, amethod))
  | Multi_found -> 
      (* is this possible? *)
      ()

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(* pre: have a unsugar AST regarding self/parent *)
let visit_and_check_funcalls find_entity prog =
  (* todo: similar to what we do in unsugar_self_parent, do this
   * unsugaring there too?
   *)
  let in_class = ref (None: string option) in

  let visitor = V.mk_visitor { V.default_visitor with

    V.kclass_def = (fun (k, _) def ->
      Common.save_excursion in_class (Some (Ast.name def.c_name)) (fun () ->
        k def
      )
    );

    V.klvalue = (fun (k,vx) x ->
      match Ast_php.untype  x with
      | FunCallSimple (callname, args)  ->
          E.find_entity_and_warn ~find_entity:(Some find_entity) 
          (Ent.Function, callname)
          +> Common.do_option (function Ast_php.FunctionE def ->
               (* todo? memoize ? *)
               let contain_func_num_args = 
                 contain_func_name_args_like (Body def.f_body) in

               if contain_func_num_args
               then pr2_once ("not checking functions containing calls to " ^
                                 "func_num_args() or alike")
               else 
                 check_args_vs_params 
                   (callname,   args +> Ast.unparen +> Ast.uncomma)
                   (def.f_name, def.f_params +> Ast.unparen +> Ast.uncomma_dots)
           | _ -> raise Impossible
           );
          k x

      | StaticMethodCallSimple (qu, name, args) ->
          (match fst qu with
          | ClassName (classname) ->
              let aclass = Ast.name classname in
              let amethod = Ast.name name in
              check_method_call (aclass, amethod) (name, args) find_entity
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
              | Some aclass ->
                  let amethod = Ast.name name in
                  check_method_call (aclass, amethod) (name, args) find_entity
              | None ->
                  (* TODO: use of $this outside class ??? *)
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
  } 
  in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* catch all the decl to grow the environment *)

let check_program2 find_entity prog = 
  visit_and_check_funcalls find_entity prog

let check_program a b = 
  Common.profile_code "Checker.functions" (fun () -> 
    check_program2 a b)
