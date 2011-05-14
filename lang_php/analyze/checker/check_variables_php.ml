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

module S = Scope_code
module Ent = Entity_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Finding stupid PHP mistakes related to variables.
 * See tests/php/scheck/variables.php for example of bugs currently
 * detected by this checker.
 * See also tests/bugs/unused_var.php for corner cases to handle.
 * 
 * This file mostly deals with scoping issues. Scoping is different
 * from typing! Those are 2 orthogonal programming language notions.
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
 * 
 * Detecting such mistakes is made slightly more complicated on one
 * side by PHP because of the lack of declaration in the language;
 * the first assgignement "declares" the variable. On the other side
 * the PHP language forces people to explicitly declared
 * the use of global variables (via the 'global' statement) which
 * helps.
 * 
 * 
 * One issue is the handling of variables passed by reference
 * which can look like use_of_undeclared_variable bugs but which
 * are not. One way to fix it is to do a global analysis that
 * remembers what are all the functions taking arguments by reference
 * and whitelist them here. But this is tedious. 
 * Another simpler way is to force programmers
 * to actually declare such variables before those kinds of
 * function calls.
 * 
 * Another issue is functions like extract(), param_get(), param_post()
 * or variable variables like $$x. Regarding the param_get/param_post()
 * one way to fix it is to just to not analyse toplevel code.
 * Another solution is to hardcode a few analysis that recognize
 * the arguments of those functions.
 * For the extract() and $$x one can just bailout of such code or
 * as evan did remember the first line where such code happen and
 * don't do any analysis pass this point.
 * 
 * Another issue is that the analysis below will probably flag lots of 
 * warning on an existing PHP codebase. Some programmers may find
 * legitimate certain things, for instance having variables declared in
 * in a foreach to escape its foreach scope. This would then hinder
 * the whole analysis because people would just not run the analysis.
 * You need the approval of the PHP developers on such analysis first
 * and get them ok to change their coding styles rules.
 * 
 * Another issue is the implicitly-declared-when-used-the-first-time
 * ugly semantic of PHP. it's ok to do  if(!($a = foo())) { foo($a) }
 * 
 * 
 * Here are some notes by evan in his own variable linter:
 * 
 * "These things declare variables in a function":
 * - DONE Explicit parameters
 * - DONE Assignment via list()
 * - DONE Static
 * - DONE Global
 * - DONE foreach()
 * - DONE catch
 * - DONE Builtins ($this)
 * - TODO Lexical vars, in php 5.3 lambda expressions
 * - SEMI Assignment
 *   (pad: variable mentionned for the first time)
 * 
 * "These things make lexical scope unknowable":
 * - TODO Use of extract()
 * - SEMI Assignment to variable variables ($$x)
 * - DONE Global with variable variables
 *   (pad: I actually bail out on such code)
 * 
 * These things don't count as "using" a variable:
 * - TODO isset()
 * - TODO empty()
 * - SEMI Static class variables
 * 
 * 
 * Here are a few additional checks and features of this checker:
 *  - when the strict_scope flag is set, check_variables will
 *    emulate a block-scoped language as in JSLint and flags
 *    variables used outside their "block".
 *  - when passed the find_entity hook, check_variables will:
 *     - know  about functions taking parameters by reference which removes
 *       some false positives.
 *     - flag use of class without a constructor or use of undefined class
 *     - use of undefined member
 *     
 * 
 * todo?: cfg analysis
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
 *
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)

(* the ref is for the number of uses *)
type environment = 
  (dname * (Scope_code.scope * int ref)) list list 

let unused_ok_when_no_strict s = 
  if !E.strict 
  then false
  else 
    List.mem s 
      ["res"; "retval"; "success"; "is_error"; "rs"; "ret";
       "e"; "ex"; "exn"; (* exception *)
      ]

let unused_ok s =     
   s =~ "_.*"
  || s = "unused"
  || s = "dummy"
  || s =~ "ignored.*"

  || unused_ok_when_no_strict s

let fake_dname s = 
  DName (s, Ast.fakeInfo s)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* Looking up a variable in the environment. 
 *  
 * May need to also return the env at this place ??? xs::zs  below ?
 * For instance when look for a parent class in scope, and 
 * then search again the parent of this parent, should start
 * from the scope at the parent place ?
 * 
 * But in fact it is worse than that because php is dynamic so 
 * should look at parent chain of things at run-time!
 * But maybe in practice for most of the codebase, people
 * dont define complex recursive class or dynamic hierarchies so
 * doing it statically in a naive way is good enough.
 *)
let rec lookup_env2 s env = 
  match env with 
  | [] -> raise Not_found
  | []::zs -> lookup_env2 s zs
  | ((a,b)::xs)::zs -> 
      if Ast_php.dname a = s 
      then b 
      else lookup_env2 s (xs::zs)
let lookup_env a b = 
  Common.profile_code "CheckVar.lookup_env" (fun () -> lookup_env2  a b)


let lookup_env_opt a b = 
  Common.optionise (fun () -> lookup_env a b)

(* This is ugly. Perhaps we should transform the environment to have
 * either a dname or a name so that those 2 types of entities
 * are clearly separated.
 *)
let rec lookup_env2_for_class s env = 
  match env with 
  | [] -> raise Not_found
  | []::zs -> lookup_env2_for_class s zs
  | ((a,b)::xs)::zs -> 
      if Ast_php.dname a = s && fst b = S.Class
      then b 
      else lookup_env2_for_class s (xs::zs)

let lookup_env_opt_for_class a b = 
  Common.optionise (fun () -> lookup_env2_for_class a b)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for now return only local vars, not class var like self::$x *)
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
let vars_assigned_in_any =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
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

let keyword_arguments_vars_in_any = 
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kargument = (fun (k, vx) x ->
      match x with
      | Arg e ->
          (match e with
          | (Assign((Var(dname, _scope), tlval_4), i_5,
                     _e),
                   t_8) ->
              Common.push2 dname aref;
              k x
          | _ ->
              k x
          )
      | ArgRef _ -> ()
    );
  })

(* maybe could be merged with vars_assigned_in but maybe we want
 * the caller to differentiate between regular assignements
 * and possibly assigned by being passed by ref
 *)
let vars_passed_by_ref_in_any ~find_entity = 
  V.do_visit_with_ref (fun aref -> 
    let params_vs_args params args = 

      let params = params +> Ast.unparen +> Ast.uncomma_dots in
      let args = 
        match args with
        | None -> []
        | Some args -> args +> Ast.unparen +> Ast.uncomma 
      in
               
      Common.zip_safe params args +> List.iter (fun (param, arg) ->
                  
        (match arg with
        | Arg 
            (Lv((Var(dname, _scope), tlval_49)), t_50) ->
            if param.p_ref <> None
            then
              Common.push2 dname aref
        | _ ->
            ()
        )
      );
    in

  { V.default_visitor with
    V.klvalue = (fun (k, vx) x ->
      match Ast.untype x with
      | FunCallSimple (name, args) ->
          E.find_entity_and_warn ~find_entity (Ent.Function, name)
          +> Common.do_option (fun id_ast ->
            match id_ast with
            | Ast_php.FunctionE def ->
                params_vs_args def.f_params (Some args)
            | _ -> raise Impossible
          );
          k x
      | FunCallVar _ 
      | StaticMethodCallSimple _
      | MethodCallSimple _
          -> 
          (* TODO !!! *)
          k x
      | _ -> 
          k x
    );
    V.kexpr = (fun (k, vx) x ->
      match Ast.untype x with
      | New (tok, class_name_ref, args) ->
          (match class_name_ref with
          | ClassNameRefStatic (ClassName name) ->
              
              E.find_entity_and_warn ~find_entity (Ent.Class, name)
              +> Common.do_option (fun id_ast ->
                match id_ast with
                | Ast_php.ClassE def ->
                    (try 
                        let constructor_def = 
                          Class_php.get_constructor def in
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
              k x

          | ClassNameRefStatic (Self _ | Parent _) ->
              (* TODO *)
              k x

          | ClassNameRefLateStatic _ ->
              (* todo ? *)
              k x
          | ClassNameRefDynamic _ ->
              (* todo ? *)
              k x
          )
      | _ -> k x
    );
    }
  )

(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* use a ref because we may want to modify it *)
let (initial_env: environment ref) = ref [
  Env_php.globals_builtins |> List.map (fun s ->
    fake_dname s, (S.Global, ref 1)
  )
]

(* opti: cache ? use hash ? *)
let _scoped_env = ref !initial_env

(* TODO use generic implem of Common ? *)
let new_scope() = _scoped_env := []::!_scoped_env 
let del_scope() = _scoped_env := List.tl !_scoped_env
let top_scope() = List.hd !_scoped_env

let add_in_scope namedef =
  let (current, older) = Common.uncons !_scoped_env in
  _scoped_env := (namedef::current)::older


(* ------------------------------------------------------------ *)
let add_binding2 k v  = 

  if !Flag.debug_checker 
  then pr2 (spf "adding binding %s" (Ast.string_of_info (Ast.info_of_dname k)));

  add_in_scope (k, v) 

let add_binding k v = 
  Common.profile_code "CV.add_binding" (fun () -> add_binding2 k v)

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let check_use_against_env var env = 
  let s = Ast.dname var in
  match lookup_env_opt s env with
  | None -> E.fatal (Ast.info_of_dname var) (E.UseOfUndefinedVariable s)
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
      if unused_ok s then ()
      else E.fatal (Ast.info_of_dname dname) (E.UnusedVariable (s, scope))
  );
  res

let do_in_new_scope_and_check_if_strict f =
  if !E.strict 
  then do_in_new_scope_and_check f
  else f ()
  
(*****************************************************************************)
(* Scoped visitor *)
(*****************************************************************************)

(* For each introduced binding (param, exception, foreach, etc), 
 * we add the binding in the environment with a counter, a la checkModule.
 *)
let visit_prog ?(find_entity=None) prog = 

  let is_top_expr = ref true in 

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

    V.kfunc_def = (fun (k, _) x ->
      do_in_new_scope_and_check (fun () -> k x);
    );
    V.kmethod_def = (fun (k, _) x ->
      match x.m_body with
      | AbstractMethod _ -> 
          (* we don't want to parameters in method interface to be counted
           * as unused Parameter
           *)
          ()
      | MethodBody _ ->
          do_in_new_scope_and_check (fun () -> k x);
    );
    V.kclass_def = (fun (k, _) x ->

      (* If inherits, then must grab the public and protected variables
       * from the parent. If find_entity raise Not_found
       * then it may be because of legitimate errors, but 
       * such error reporting will be done in check_class_php.ml
       *)
      x.c_extends +> Common.do_option (fun (tok, name_class_parent) ->
        (* todo: ugly to have to "cast" *)

        E.find_entity_and_warn ~find_entity (Ent.Class, name_class_parent)
        +> Common.do_option (fun id_ast ->
          (match id_ast with
          | Ast_php.ClassE def2 ->
              
              let vars = Class_php.get_public_or_protected_vars_of_class def2
              in
              (* we put 1 as use_count because we are not interested
               * in error message related to the parent.
               * Moreover it's legitimate to not use the parent
               * var in a children class
               *)
              vars +> List.iter (fun dname ->
                add_binding dname (S.Class, ref 1);
              )

          | _ -> raise Impossible
          )
      ));

      (* must reorder the class_stmts as we want class variables defined
       * after the method to be considered first for scope/variable
       * analysis
       *)
      let x' = Class_php.class_variables_reorder_first x in
      
      do_in_new_scope_and_check (fun () -> k x');
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

      | StaticVars (_, vars_list, _) ->
          vars_list |> Ast.uncomma |> List.iter (fun (varname, affect_opt) ->
            add_binding varname (S.Static, ref 0);
            (* TODO recurse on the affect *)
          )

      | TypedDeclaration (_, _, _, _) ->
          pr2_once "TODO: TypedDeclaration"

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
      add_binding x.p_name (S.Param, ref 0);
      k x
    );

    V.kcatch = (fun (k,vx) x ->
      let (_t, (_, (classname, dname), _), stmts) = x in
      (* could use local ? could have a UnusedExceptionParameter ? *)
      do_in_new_scope_and_check (fun () -> 
        add_binding dname (S.LocalExn, ref 0);
        k x;
      );
    );

    V.kclass_stmt = (fun (k, _) x ->
      match x with
      | ClassVariables (_modifier, _opt_ty, class_vars, _tok) ->
          (* could use opt_ty to do type checking but this is better done
           * in typing_php.ml *)

          (* todo: add the modifier in the scope ? *)
          class_vars |> Ast.uncomma |> List.iter (fun (dname, affect_opt) ->
            add_binding dname (S.Class, ref 0);
            (* todo? recurse on the affect *)
          )
      | _ -> k x
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

      | ObjAccessSimple (lval, tok, name) ->
          (match Ast.untype lval with
          | This _ ->
              (* access to $this->fld, have to look for $fld in
               * the environment, with a Class scope.
               *
               * This is mostly a copy-paste of check_use_against_env
               * but we additionnally check the scope.
               * 
               * TODO: but to be correct it requires entity_finder
               * because the field may have been defined in the parent.
               *)
              let s = Ast.name name in
              (match lookup_env_opt_for_class s !_scoped_env with
              | None -> 
                  E.fatal (Ast.info_of_name name) (E.UseOfUndefinedMember s)

              | Some (scope, aref) ->
                  if (scope <> S.Class)
                  then 
                    pr2 ("WEIRD, scope <> Class, " ^ (Ast.string_of_info tok));
                    
                  incr aref
              )
          | _ -> k x
          )
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

      | Eval _ -> pr2_once "Eval: TODO";
          k x

      | Lambda def ->

          (* reset completely the environment *)
          Common.save_excursion _scoped_env !initial_env (fun () ->
            Common.save_excursion is_top_expr true (fun () ->
            do_in_new_scope_and_check (fun () ->

              def.l_use +> Common.do_option (fun (_tok, vars) ->
                vars +> Ast.unparen +> Ast.uncomma +> List.iter (function
                | LexicalVar (_is_ref, dname) ->
                    add_binding dname (S.Closed, ref 0)
                );
              );
              k x
            ))
          )
          
      (* Include | ... ? *)
          
      | _ ->

       (* do the checking and environment update only for the top expressions *)
       if !is_top_expr
       then begin
        is_top_expr := false;
      
        let used = vars_used_in_any (Expr x) in
        let assigned = vars_assigned_in_any (Expr x) in
        let passed_by_refs = vars_passed_by_ref_in_any ~find_entity (Expr x) in

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

        used' |> List.iter (fun v -> check_use_against_env v !_scoped_env);

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
  visitor (Program prog)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_and_annotate_program2 ?find_entity prog =

  (* globals (re)initialialisation *) 
  _scoped_env := !initial_env;

  visit_prog ?find_entity prog;
  ()

let check_and_annotate_program ?find_entity a = 
  Common.profile_code "Checker.variables" (fun () -> 
    check_and_annotate_program2 ?find_entity a)
