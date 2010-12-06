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
 * history: repeat what iain wanted for his Strict mode that I first
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

(* This is ugly. Some of the code requires to have a 'name' type
 * for every "entities" we are defining and checking. For a class 
 * constant we should really have a pair of name, one for the class
 * and one for the constant itself. Instead we abuse 'name' and
 * pack into it also the classname.
 *)
let rewrap_name_with_class_name classname name =
  match name with 
  | Name (s, info) ->
      let new_str = spf "%s::%s" classname s in
      Name (new_str, Ast.rewrap_str new_str info)
  (* only classnames can be a XhpName. Constants (or functions)
   * are always simple names
   *)
  | XhpName _ ->
      failwith "Impossible: only classes can be XhpName"

let mk_class_name s info = 
  Name (s, info)

(* todo? move in class_php.ml ? *)
let resolve_class_name qu in_class =
  match qu, in_class with
  | Qualifier (name, _tok), _ ->
      name
  | Self _, Some (name, _parent) ->
      name
  | Self _, None ->
      failwith ("Use of self:: outside of a class")
  | Parent _, (Some (_, Some parent)) -> 
      parent
  | Parent _, _ ->
      failwith "Use of parent:: in a class without a parent"

(*****************************************************************************)
(* Typing rules *)
(*****************************************************************************)
(* cf also type_php.ml *)

let no_check_when_contain = [
  "func_num_args";
  "func_get_args";
]

let contain_func_name_args_like any =
  let funcalls = Lib_parsing_php.get_all_funcalls_any any in
  no_check_when_contain +> List.exists (fun danger_func -> 
    List.mem danger_func funcalls
  )


let check_args_vs_params (callname, all_args) (defname, all_params) =

  let info = Ast_php.info_of_name callname in

  let rec aux args params = 
    match args, params with
    | [], [] -> ()
    | [], y::ys ->
        if y.p_default = None 
        then E.fatal (E.NotEnoughArguments (info, defname))
        else aux [] ys
    | x::xs, [] ->
        E.fatal (E.TooManyArguments (info, defname))
    | x::xs, y::ys ->
        (match x with
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
              E.fatal (E.WrongKeywordArgument(dn, y, severity))
        | _ -> ()
        );
        aux xs ys
  in
  aux all_args all_params

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

let visit_and_check_funcalls  ?(find_entity = None) prog =

  let in_class = ref (None: (Ast.name * Ast.name option) option) in

  let hooks = { Visitor_php.default_visitor with

    V.kclass_def = (fun (k, _) def ->
      let classname = def.c_name in
      let parent_opt = 
        match def.c_extends with
        | None -> None
        | Some (tok, classname) -> Some classname
      in
      Common.save_excursion in_class (Some (classname, parent_opt)) (fun () ->
        def.c_extends +> Common.do_option (fun (tok, classname) ->
          (* todo? E.find_entity ~find_entity (Entity_php.Class classname) *)
          ()
        );
        k def;
      );
    );


    Visitor_php.klvalue = (fun (k,vx) x ->
      match Ast_php.untype  x with
      | FunCallSimple (callname, args)  ->
          E.find_entity ~find_entity (Entity_php.Function, callname)
          +> Common.do_option (fun id_ast ->
           match id_ast with
           | Ast_entity_php.Function def ->
               (* todo? memoize ? *)
               let contain_func_num_args = 
                 contain_func_name_args_like (Body def.f_body) in

               if contain_func_num_args
               then pr2_once ("not checking functions containing calls to " ^
                                 "func_num_args() or alike")
               else 
                 check_args_vs_params 
                   (callname,   args +> Ast.unparen +> Ast.uncomma)
                   (def.f_name, def.f_params +> Ast.unparen +> Ast.uncomma)
           | _ -> raise Impossible
           );
          k x

      | StaticMethodCallSimple (qu, callname, args) ->
          let classname = resolve_class_name qu !in_class in
          let sclassname = Ast.name classname in
          let name' = rewrap_name_with_class_name sclassname callname in
          E.find_entity ~find_entity (Entity_php.StaticMethod, name')
          +> Common.do_option (fun id_ast ->
            match id_ast with
            | Ast_entity_php.Method def ->

               let contain_func_num_args = 
                 contain_func_name_args_like (ClassStmt (Method def)) in

               if contain_func_num_args
               then pr2_once ("not checking functions containing calls to " ^
                                 "func_num_args() or alike")
               else 

                check_args_vs_params 
                  (callname,   args +> Ast.unparen +> Ast.uncomma)
                  (def.m_name, def.m_params +> Ast.unparen +> Ast.uncomma)
                
            | _ -> raise Impossible
          )

      | FunCallVar _ -> 
          pr2 "TODO: handling FuncVar";
          k x
      | _ -> k x
    );
  } in
  let visitor = Visitor_php.mk_visitor hooks in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* catch all the decl to grow the environment *)

let check_program2 ?find_entity prog = 
  visit_and_check_funcalls ?find_entity prog


let check_program ?find_entity a = 
  Common.profile_code "Checker.functions" (fun () -> 
    check_program2 ?find_entity a)
