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

module S = Scope_code
module Ent = Entity_php

open Env_check_php
open Check_variables_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module helps find stupid PHP mistakes related to variables. See
 * tests/php/scheck/variables.php for examples of bugs currently
 * detected by this checker. This module checks but also annotates
 * the AST with scoping information as a side effect. This is useful
 * in Codemap to display differently references to paramaters, local vars,
 * global vars, etc.
 * 
 * This file mostly deals with scoping issues. Scoping is different
 * from typing! Those are two orthogonal programming language concepts.
 * Some similar checks are done by JSlint.
 * 
 * This file is concerned with variables, that is Ast_php.dname
 * entities, so for completness C-s for dname in ast_php.ml and
 * see if all uses of it are covered. Other files are more concerned
 * about checks related to entities, that is Ast_php.name.
 * 
 * The errors detected here are mostly:
 *  - UseOfUndefinedVariable
 *  - UnusedVariable
 * 
 * Detecting such mistakes is made slightly more complicated
 * by PHP because of the lack of declaration in the language;
 * the first assgignement "declares" the variable. On the other side
 * the PHP language forces people to explicitly declared
 * the use of global variables (via the 'global' statement) which
 * makes other things easier.
 * 
 * One important issue is the handling of variables passed by reference
 * which can look like UseOfUndefinedVariable bugs but which
 * are not. One way to fix it is to do a global analysis that
 * remembers what are all the functions taking arguments by reference
 * and whitelist them here. But it has a cost.
 * Another way is to force programmers to actually declare such variables
 * before those kinds of function calls.
 * 
 * Another issue is functions like extract(), param_get(), param_post()
 * or variable variables like $$x. Regarding the param_get/param_post()
 * one way to fix it is to just not analyse toplevel code.
 * Another solution is to hardcode a few analysis that recognizes
 * the arguments of those functions.
 * For the extract() and $$x one can just bailout of such code or
 * as evan did remember the first line where such code happen and
 * don't do any analysis pass this point.
 * 
 * Another issue is that the analysis below will probably flag lots of 
 * warnings on an existing PHP codebase. Some programmers may find
 * legitimate certain things, for instance having variables declared in
 * a foreach to escape its foreach scope. This would then hinder
 * the whole analysis because people would just not run the analysis.
 * You need the approval of the PHP developers on such analysis first
 * and get them ok to change their coding styles rules.
 * 
 * Another issue is the implicitly-declared-when-used-the-first-time
 * ugly semantic of PHP. it's ok to do  if(!($a = foo())) { foo($a) }
 * 
 * Here are some notes by evan in his own variable linter:
 * 
 * "These things declare variables in a function":
 * - DONE Explicit parameters
 * - DONE Assignment via list()
 * - DONE Static, Global
 * - DONE foreach()
 * - DONE catch
 * - DONE Builtins ($this)
 * - TODO Lexical vars, in php 5.3 lambda expressions
 * - SEMI Assignment
 *   (pad: variable mentionned for the first time)
 * 
 * "These things make lexical scope unknowable":
 * - DONE Use of extract()
 * - SEMI Assignment to variable variables ($$x)
 * - DONE Global with variable variables
 *   (pad: I actually bail out on such code)
 * 
 * These things don't count as "using" a variable:
 * - TODO isset()
 * - TODO empty()
 * - SEMI Static class variables
 * 
 * Here are a few additional checks and features of this checker:
 *  - when the strict_scope flag is set, check_variables will
 *    emulate a block-scoped language as in JSLint and flags
 *    variables used outside their "block".
 *  - when passed the find_entity hook, check_variables will:
 *     - know about functions taking parameters by refs, which removes
 *       some false positives
 *     - flag uses of class without a constructor or use of undefined classes
 *     - use of undefined members
 * 
 * todo? create generic function extracting set of defs
 *  and set of uses regarding variable ? and make connection ?
 *  If connection empty => unused var
 *  If no binding => undefined variable
 * 
 * todo? maybe should use the PIL here; it would be 
 * less tedious to write such analysis. There is too
 * many kinds of statements and expressions. Can probably factorize
 * more the code. For instance list($a, $b) = array is sugar 
 * for some assignations. But at the same time we may want to do special error 
 * reports for the use of list, for instance it's ok to not use
 * every var mentionned in a list(), but it's not to not use 
 * a regular assigned variable. So maybe not a good idea to have PIL.
 * 
 * quite some copy paste with check_functions_php.ml :(
 * 
 * history:
 *  - sgrimm had the simple idea of just counting the number of occurences
 *    of variables in a program, at the token level. If only 1, then
 *    probably a typo. But sometimes variable names are mentionned in
 *    interface signature in which case they occur only once. So you need
 *    some basic analysis, the token level is not enough. You may not
 *    need the CFG but at least you need the AST to differentiate the 
 *    different kinds of unused variables. 
 * 
 *  - Some of the scoping logic was previously in another file, scoping_php.ml
 *    But we were kind of duplicating the logic that is in scoping_php.ml 
 *    PHP has bad scoping rule allowing variable declared through a foreach
 *    to be used outside the foreach, which is probably wrong. 
 *    Unfortunately, knowing from scoping_php.ml just the status of a
 *    variable (local, global, or param) is not good enough to find bugs 
 *    related to those weird scoping rules. So I've put all variable scope
 *    related stuff in this file and removed the duplication in scoping_php.ml
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)
(* see env_check_php.ml *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* see check_variables_helpers_php.ml *)

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let check_use_against_env ~in_lambda ~has_extract var env = 
  let s = Ast.dname var in
  match lookup_env_opt s env with
  | None ->
      (* todo? could still issue an error but with the information that
       * there was an extract around?
       *)
      if has_extract
      then ()
      else 
       E.fatal (Ast.info_of_dname var) 
        (if in_lambda 
        then (E.UseOfUndefinedVariableInLambda s)
        else (E.UseOfUndefinedVariable s)
        )
  | Some (scope, aref) -> incr aref

let do_in_new_scope_and_check f = 
  new_scope();
  let res = f() in

  let top = top_scope () in
  del_scope();

  top |> List.rev |> List.iter (fun (dname, (scope, aref)) ->
    if !aref = 0 
    then 
      let s = Ast.dname dname in
      if Env_check_php.unused_ok s then ()
      else E.fatal (Ast.info_of_dname dname) (E.UnusedVariable (s, scope))
  );
  res

let do_in_new_scope_and_check_if_strict f =
  if !E.strict 
  then do_in_new_scope_and_check f
  (* use same scope *)
  else f ()
  
(*****************************************************************************)
(* Scoped visitor *)
(*****************************************************************************)

(* For each introduced binding (param, exception, foreach, etc), 
 * we add the binding in the environment with a counter, a la checkModule.
 * We then check at use time if something was declared before. We then
 * finally check when we exit a scope that all variables were actually used.
 *)
let visit_prog find_entity prog = 

  (* ugly: have to use all those save_excursion below because of 
   * the visitor interface which is imperative. But threading an 
   * environment is also tedious so maybe not too ugly.
   *)
  let is_top_expr = ref true in 
  let in_lambda = ref false in
  let in_class = ref None in
  let has_extract = ref false in
  let scope = ref Ent.StmtList in

  let visitor = Visitor_php.mk_visitor { Visitor_php.default_visitor with

    (* scoping management.
     * 
     * if, while, and other blocks should introduce a new scope. 
     * the default function-only scope of PHP is a bad idea. 
     * Jslint thinks the same.
     * 
     * Even if PHP has no good scoping rules, I want to force programmers
     * like in Javascript to write code that assumes good scoping rules
     * 
     * Note that this is will just create a scope and check for the
     * body of functions. You also need a do_in_new_scope_and_check
     * for the function itself which can have parameter that we
     * want to check.
     *)
    V.kstmt_and_def_list_scope = (fun (k, _) x ->
      do_in_new_scope_and_check_if_strict (fun () -> k x)
    );

    (* regular function scope check at least *)
    V.kfunc_def = (fun (k, _) x ->
      Common.save_excursion has_extract false (fun () ->
      Common.save_excursion scope Ent.Function (fun () ->
        do_in_new_scope_and_check (fun () -> k x);
      ))
    );
    V.kmethod_def = (fun (k, _) x ->
      match x.m_body with
      | AbstractMethod _ -> 
          (* we don't want to parameters in method interface to be counted
           * as unused Parameter
           *)
          ()
      | MethodBody _ ->
      (* todo: diff between Method and StaticMethod? *)
      Common.save_excursion has_extract false (fun () ->
      Common.save_excursion scope Ent.Method (fun () ->
        do_in_new_scope_and_check (fun () -> 
          if not (Class_php.is_static_method x)
          then begin
            (* we put 1 as use_count because we are not interested
             * in error message related to $this.
             * it's legitimate to not use $this in a method
             *)
            let dname = Ast.DName ("this", Ast.fakeInfo "this") in
            add_binding dname (S.Class, ref 1);
          end;
          k x
        );
      ))
    );
    V.kclass_def = (fun (k, _) x ->
      Common.save_excursion in_class (Some (Ast.name x.c_name)) (fun () ->
        do_in_new_scope_and_check (fun () -> 
          k x
        )
      );
    );

    (* Introduce a new scope for StmtList ? this would forbid user to 
     * have some statements, a func, and then more statements
     * that share the same variable. Toplevel statements
     * should not be mixed with function definitions (excepts
     * for the include/require at the top, which anyway are
     * not really, and should not be statements)
     *)

    (* adding defs of dname in environment *)

    V.kstmt = (fun (k, vx) x ->
      match x with
      | Globals (_, vars_list, _) ->
          vars_list |> Ast.uncomma |> List.iter (fun var ->
            match var with
            | GlobalVar dname -> 
                add_binding dname (S.Global, ref 0)
            | GlobalDollar (tok, _)  | GlobalDollarExpr (tok, _) ->
                E.warning tok E.UglyGlobalDynamic
          )

      (* mostly copy paste of ./pfff -dump_php tests/php/scheck/endpoint.php 
       * facebook specific? should be a hook instead to visit_prog?
       *)
      | ExprStmt(
         (Lv(
          (FunCallSimple(Name((("param_post" | "param_get" | "param_request")
                                  as kind, i_1)),
            (i_2,
             (Left(
                Arg(
                  (ConsArray(i_3, (i_4, array_args, i_20)),
                   t_21))))::rest_param_xxx_args,
             i_22)),
          tlval_23)),
         t_24), i_25) ->

          let array_args = Ast.uncomma array_args in
          let rest_param_xxx_args = Ast.uncomma rest_param_xxx_args in

          (* have passed a 'prefix' arg, or nothing *)
          if List.length rest_param_xxx_args <= 1
          then begin
              let prefix_opt =
                match rest_param_xxx_args with
                | [Arg (Sc(C(String(str_prefix, tok_prefix))),_t)] -> 
                    Some str_prefix
                | [] ->
                    (match kind with
                    | "param_post" -> Some "post_"
                    | "param_get" -> Some "get_"
                    | "param_request" -> Some "req_"
                    | _ -> raise Impossible
                    )
                | _ -> 
                    (* display an error? weird argument to param_xxx func? *)
                    None
              in
              prefix_opt +> Common.do_option (fun prefix ->
               array_args +> List.iter (function
               | ArrayArrowExpr((Sc(C(String((param_string, tok_param)))), t_6),
                               i_7,
                               _typ_param) ->
                   let dname = DName (prefix ^ param_string, tok_param) in
                   add_binding dname (S.Local, ref 0);
               | _ -> ()
               );
              )
          end;
          k x

      | StaticVars (_, vars_list, _) ->
          vars_list |> Ast.uncomma |> List.iter (fun (varname, affect_opt) ->
            add_binding varname (S.Static, ref 0);
            (* TODO recurse on the affect *)
          )

      | TypedDeclaration (_, _, _, _) ->
          (* was a PHP extension I made with dougli and asuhan during a
           * hackathon but it's not there anymore
           *)
          ()

      | Foreach (tok, _, e, _, var_either, arrow_opt, _, colon_stmt) ->
          vx (Expr e);

          let lval = 
            match var_either with
            | Left (is_ref, var) -> 
                var
            | Right lval ->
                lval
          in
          (match Ast.untype lval with
          | Var (dname, scope_ref) ->
              do_in_new_scope_and_check_if_strict (fun () ->
                (* People often use only one of the iterator when
                 * they do foreach like   foreach(... as $k => $v).
                 * We want to make sure that at least one of 
                 * the iterator is used, hence this trick to
                 * make them share the same ref.
                 *)
                let shared_ref = ref 0 in
                add_binding dname (S.LocalIterator, shared_ref);

                scope_ref := S.LocalIterator;

                (match arrow_opt with
                | None -> ()
                | Some (_t, (is_ref, var)) -> 
                    (match Ast.untype var with
                    | Var (dname, scope_ref) ->
                        add_binding dname (S.LocalIterator, shared_ref);

                        scope_ref := S.LocalIterator;

                    | _ ->
                        E.warning tok E.WeirdForeachNoIteratorVar
                    );
                );
                
                vx (ColonStmt2 colon_stmt);
              );

          | _ -> 
              E.warning tok E.WeirdForeachNoIteratorVar
          )
      (* note: I was not handling Unset which takes a lvalue (not
       * an expression) as an argument. Because of that the kexpr
       * hook below that compute the set of vars_used_in_expr
       * was never triggered and every call to unset($avar) was not
       * considered as a use of $avar.
       * 
       * C-s "lvalue" in Ast_php to be sure all Ast elements
       * containing lvalue are appropriatly considered.
       * 
       * In the send I still decided to not consider it because
       * a mention of a variable in a $unset should not be really
       * considered as a use of variable. There should be another
       * statement in the function that actually use the variable.
       *)
      | Unset (t1, lvals_list, t2) ->
          k x
      | _ -> k x
    );

    V.kparameter = (fun (k,vx) x ->
      let cnt = 
        match !scope with
        (* Don't report UnusedParameter for parameters of methods.
         * people sometimes override a method and don't use all
         * the parameters
         *)
        | Ent.Method -> 1
        | Ent.Function -> 0
        | _ -> 0
      in
      add_binding x.p_name (S.Param, ref cnt);
      k x
    );

    V.kcatch = (fun (k,vx) x ->
      let (_t, (_, (classname, dname), _), stmts) = x in
      (* The scope of catch is actually also at the function level in PHP ...
       * but for this one it is so ugly that I introduce a new scope
       * even outside strict mode. It's just too ugly
       * 
       * todo?could use local ? could have a UnusedExceptionParameter ? 
       *)
      do_in_new_scope_and_check (fun () -> 
        add_binding dname (S.LocalExn, ref 0);
        k x;
      );
    );

    (* uses *)

    V.klvalue = (fun (k,vx) x ->
      match Ast.untype x with
      (* the checking is done upward, in kexpr, and only for topexpr *)
      | Var (dname, scope_ref) ->
          (* assert scope_ref = S.Unknown ? *)
          let s = Ast.dname dname in
    
          (match lookup_env_opt s !_scoped_env with
          | None -> 
              scope_ref := S.Local;
          | Some (scope, _) ->
              scope_ref := scope;
          )

      | FunCallSimple (Name ("extract", _), _args) ->
          has_extract := true;
          k x

      | _ -> 
          k x
    );

    V.kexpr = (fun (k, vx) x ->
      match Ast.untype x with
      (* todo? if the ConsList is not at the toplevel, then 
       * the code below will be executed first, which means
       * vars_used_in_expr will wrongly think vars in list() expr
       * are unused var. But this should be rare because list()
       * should be used at a statement level!
       *)
      | AssignList (_, xs, _, e) ->
          let assigned = xs |> Ast.unparen |> Ast.uncomma in

          (* Use the same trick than for LocalIterator, make them 
           * share the same ref
           *)
          let shared_ref = ref 0 in

          assigned |> List.iter (fun list_assign ->
            let vars = vars_used_in_any (ListAssign list_assign) in
            vars |> List.iter (fun v ->
              (* if the variable was already existing, then 
               * better not to add a new binding cos this will mask
               * a previous one which will never get its ref 
               * incremented.
               *)
              let s = Ast.dname v in
              match lookup_env_opt s !_scoped_env with
              | None ->
                  add_binding v (S.ListBinded, shared_ref)
              | Some _ ->
                  ()
            );
          );
          vx (Expr e)

      | Eval _ -> 
          pr2_once "Eval: TODO";
          k x

      | Lambda def ->

          (* reset completely the environment *)
          Common.save_excursion _scoped_env !initial_env (fun () ->
          Common.save_excursion in_lambda true (fun () ->
          Common.save_excursion has_extract false (fun () ->
          Common.save_excursion is_top_expr true (fun () ->
            do_in_new_scope_and_check (fun () ->

              def.l_use +> Common.do_option (fun (_tok, vars) ->
                vars +> Ast.unparen +> Ast.uncomma +> List.iter (function
                | LexicalVar (_is_ref, dname) ->
                    add_binding dname (S.Closed, ref 0)
                );
              );
              k x
            ))))
          )
      (* Include | ... ? *)
          
      | _ ->

       (* do the checking and environment update only for the top expressions *)
       if !is_top_expr
       then begin
        is_top_expr := false;
      
        let used = 
          vars_used_in_any (Expr x) in
        let assigned = 
          vars_assigned_in_any (Expr x) in
        let passed_by_refs =
          match find_entity with
          (* can't do much :( *)
          | None -> []
          | Some finder ->
              vars_passed_by_ref_in_any ~in_class:!in_class finder (Expr x)
        in

        (* keyword arguments should be ignored and treated as comment *)
        let keyword_args = keyword_arguments_vars_in_any (Expr x) in

        (* todo: enough ? if do $x = $x + 1 then got problems ? *)
        let used' = 
          used |> Common.exclude (fun v -> 
            List.mem v assigned || List.mem v passed_by_refs 
             (* actually passing a var by def can also mean it's used
              * as the variable can be a 'inout', but this special
              * case is handled specifically for passed_by_refs
              *)
          )
        in
        let assigned' = 
          assigned |> Common.exclude (fun v -> List.mem v keyword_args) in

        used' |> List.iter (fun v -> 
          check_use_against_env 
            ~in_lambda:!in_lambda ~has_extract:!has_extract
            v !_scoped_env);

        assigned' |> List.iter (fun v -> 

          (* Maybe a new local var. add in which scope ?
           * I would argue to add it only in the current nested
           * scope. If someone wants to use a var outside the block,
           * he should have initialized the var in the outer context.
           * Jslint does the same.
           *)
          let s = Ast.dname v in
          (match lookup_env_opt s !_scoped_env with
          | None -> 
              add_binding v (S.Local, ref 0);
          | Some _ ->
              (* Does an assignation counts as a use ? if you only 
               * assign and never use a variable, what is the point ? 
               * This should be legal only for parameters (note that here
               * I talk about parameters, not arguments)
               * passed by reference.
               *)
              ()
          )
        );
        passed_by_refs |> List.iter (fun v -> 
          (* Maybe a new local var *)
          let s = Ast.dname v in
          (match lookup_env_opt s !_scoped_env with
          | None -> 
              add_binding v (S.Local, ref 0);
          | Some (scope, aref) ->
              (* was already assigned and passed by refs, 
               * increment its use counter then
               *)
              incr aref;
          )
        );
        k x;
        is_top_expr := true;
      end
       else
         (* still recurse when not in top expr *)
         k x
    );
  }
  in
  (* we must check if people used the variables declared in the toplevel
   * context or the param_post/param_get variables, hence the 
   * do_in_new_scope_and_check below.
   *)
  do_in_new_scope_and_check (fun () -> 
    visitor (Program prog)
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_and_annotate_program2 find_entity prog =

  (* globals (re)initialialisation *) 
  _scoped_env := !initial_env;

  visit_prog find_entity prog

let check_and_annotate_program a b = 
  Common.profile_code "Checker.variables" (fun () -> 
    check_and_annotate_program2 a b)
