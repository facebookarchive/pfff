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
module E = Error_php
module S = Scope_code
module Flag = Flag_analyze_php
module V = Visitor_php
module Ent = Entity_php

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* returns only local vars, not class vars like self::$x. For class vars
 * see check_classes_php.ml.
 *)
let vars_used_in_any x =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k, vx) x ->
      match Ast.untype x with
      | Lambda def -> 
          (* stop here, do not recurse in but count the use(...) as vars used *)
          def.l_use +> Common.do_option (fun (_tok, vars) ->
            vars +> Ast.unparen +> Ast.uncomma +> List.iter (function
            | LexicalVar (_is_ref, dname) ->
                Common.push2 dname aref
            );
          )
      | _ -> k x
    );

    V.klvalue = (fun (k,vx) x ->
      match Ast.untype x with
      | Var (dname, _scope) ->
          Common.push2 dname aref

      (* Used to have a bad representation for A::$a[e]
       * It was parsed asa VQualifier(VArrayAccesS($a, e))
       * but e could contains variable too !! so should actually
       * visit the lval. But we also don't want to visit certain
       * parts of lval.
         * Introducing ClassVar fixed the problem.
       * The qualifier should be with Var, just like what I do for name.
       * 
       * old: | VQualifier (qu, lval) -> ()
       *)

      (* transform This into a Var *)
      | This (tok) ->
          let dname = Ast.DName("this", tok) in
          Common.push2 dname aref
            
      | _ -> 
          k x
    );
  }) x
    
(* TODO: qualified_vars_in !!! *)

(* the lval can be an array expression like 
 * $arr[$v] = 2;  in which case we must consider
 * only $arr in the list of assigned var, not $v.
 * 
 * old: let vars = vars_used_in_lvalue lval in
 * 
 *)
let rec get_assigned_var_lval_opt lval = 

  (match Ast.untype lval with
  | Var (dname,  _) ->
      Some dname
  | VArrayAccess (lval, e) ->
      get_assigned_var_lval_opt lval
  (* TODO *)
  | _ -> None
  )

(* update: does consider also function calls to function taking parameters via
 * reference. Use global info.
 *)
let vars_assigned_in_any any =
  any +> V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k,vx) x ->
      match Ast.untype x with
      | Assign (lval, _, _) 
      | AssignOp (lval, _, _) 
          
      | AssignRef (lval, _, _, _) 
      | AssignNew (lval, _, _, _, _,  _) ->
          
          let vopt = get_assigned_var_lval_opt lval in
          vopt |> Common.do_option (fun v -> Common.push2 v aref);
          
          (* recurse, can have $x = $y = 1 *)
          k x
      | _ -> 
          k x
    );
    }
  )

let keyword_arguments_vars_in_any any = 
  any +> V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kargument = (fun (k, vx) x ->
      match x with
      | Arg e ->
          (match e with
          | (Assign((Var(dname, _scope), tlval_4), i_5, _e), t_8) ->
              Common.push2 dname aref;
          | _ -> ()
          );
          k x
      | ArgRef _ -> ()
    );
  })

(*****************************************************************************)
(* Vars passed by ref *)
(*****************************************************************************)
(* 
 * This is complicated in PHP because one does not have to use &$var
 * at the call site in PHP ... This is ugly ... So to detect variables
 * passed by reference, we need to look at the definition of the 
 * function or method called, hence the find_entity argument.
 * 
 * less: maybe could be merged with vars_assigned_in but maybe we want
 * the caller to differentiate between regular assignements
 * and possibly assigned by being passed by ref
 * 
 * todo: factorize code, at least for the methods and new calls.
 *)
let vars_passed_by_ref_in_any ~in_class find_entity = 
  V.do_visit_with_ref (fun aref -> 
    let params_vs_args params args = 

      let params = params +> Ast.unparen +> Ast.uncomma_dots in
      let args = 
        match args with
        | None -> []
        | Some args -> args +> Ast.unparen +> Ast.uncomma 
      in

      (* maybe the #args does not match #params, but this is not our
       * business here; this will be detected anyway by check_functions or
       * check_class
       *)
      Common.zip_safe params args +> List.iter (fun (param, arg) ->
        match arg with
        | Arg 
            (Lv((Var(dname, _scope), tlval_49)), t_50) ->
            if param.p_ref <> None
            then Common.push2 dname aref
        | _ -> ()
      );
    in
    
  { V.default_visitor with
    V.klvalue = (fun (k, vx) x ->
      match Ast.untype x with
      (* quite similar to code in check_functions_php.ml *)

      | FunCallSimple (name, args) ->
          let s = Ast.name name in
          (match s with
          (* special case, ugly but hard to do otherwise *)
          | "sscanf" -> 
              (match args +> Ast.unparen +> Ast.uncomma with
              | x::y::vars ->
                  vars +> List.iter (fun arg ->
                    match arg with
                    | Arg (Lv((Var(dname, _scope), tlval_49)), t_50) ->
                        Common.push2 dname aref
                    (* todo? wrong, it should be a variable *)
                    | _ -> ()
                  )
              (* wrong number of arguments, not our business, it will
               * be detected by another checker anyway
               *)
              | _ -> ()
              )
          | s -> 
           E.find_entity_and_warn find_entity (Ent.Function, name) 
             (function Ast_php.FunctionE def ->
                params_vs_args def.f_params (Some args)
             | _ -> raise Impossible
           )
          );
          k x
      | StaticMethodCallSimple (qu, name, args) ->
          (match qu with
          | ClassName (classname), _ ->
              let aclass = Ast.name classname in
              let amethod = Ast.name name in
              (try 
                  let def = 
                    Class_php.lookup_method (aclass, amethod) find_entity in
                  params_vs_args def.m_params (Some args)
                with 
                (* could not find the method, this is bad, but
                 * it's not our business here; this error will
                 * be reported anyway in check_functions_php.ml anyway
                 *)
                | Not_found | Multi_found  -> ()
                )
           | (Self _ | Parent _), _ ->
               failwith "check_var_help: call unsugar_self_parent()"
           | LateStatic _, _ ->
               (* not much we can do :( *)
               ()
          );
          k x

      | MethodCallSimple (lval, _tok, name, args) ->
          (match Ast.untype lval with
          (* if this-> then can use lookup_method.
           * Being complete and handling any method calls like $o->foo()
           * requires to know what is the type of $o which is quite
           * complicated ... so let's skip that for now.
           * 
           * todo: special case also id(new ...)-> ?
           *)
          | This _ ->
              (match in_class with
              | Some aclass ->
                let amethod = Ast.name name in
                (try 
                  let def = 
                    Class_php.lookup_method (aclass, amethod) find_entity in
                  params_vs_args def.m_params (Some args)
                  with 
                  | Not_found | Multi_found  -> ()
                )
              | None ->
                  (* wtf? use of $this outside a class? *)
                  ()
              )
          | _ -> ()
          );
          k x 

      | FunCallVar _ -> 
          (* can't do much *)
          k x
      | _ -> 
          k x
    );
    V.kexpr = (fun (k, vx) x ->
      (match Ast.untype x with
      | New (tok, class_name_ref, args) ->
          (match class_name_ref with
          | ClassNameRefStatic (ClassName name) ->
              (* todo: use lookup_method there too ! *)
              E.find_entity_and_warn find_entity (Ent.Class, name)
              (function Ast_php.ClassE def ->
                (try 
                    let constructor_def = Class_php.get_constructor def in
                    params_vs_args constructor_def.m_params args
                  with Not_found ->
                   (* TODO: too many FP for now
                       if !Flag.show_analyze_error
                       then pr2_once (spf "Could not find constructor for: %s" 
                                         (Ast.name name));
                   *)
                   ()
                );
              | _ -> raise Impossible
              );
          | ClassNameRefStatic (Self _ | Parent _) ->
              failwith "check_functions_php: call unsugar_self_parent()"

          (* can't do much *)
          | ClassNameRefDynamic _  | ClassNameRefStatic (LateStatic _) -> ()
          )
      | _ -> ()
      );
      k x
    );
    }
  )
