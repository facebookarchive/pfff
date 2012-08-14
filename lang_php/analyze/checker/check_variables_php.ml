(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
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
module E = Error_php
module S = Scope_code
module Ent = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module helps find stupid PHP mistakes related to variables. See
 * tests/php/scheck/variables.php for examples of bugs currently
 * detected by this checker. This module not only checks but also annotates
 * the AST with scoping information as a side effect. This is useful
 * in codemap to display differently references to parameters, local vars,
 * global vars, etc.
 * 
 * This file mostly deals with scoping issues. Scoping is different
 * from typing! Those are two orthogonal programming language concepts.
 * old: This file is concerned with variables, that is Ast_php.dname
 *  entities, so for completness C-s for dname in ast_php.ml and
 *  see if all uses of it are covered. Other files are more concerned
 *  about checks related to entities, that is Ast_php.name.
 * 
 * The errors detected here are mostly:
 *  - UseOfUndefinedVariable
 *  - UnusedVariable
 * Some similar checks are done by JSlint.
 * 
 * Some issues:
 *  - detecting variable-related mistakes is made slightly more complicated
 *    by PHP because of the lack of declaration in the language;
 *    the first assignment "declares" the variable (on the other side
 *    the PHP language forces people to explicitly declared
 *    the use of global variables (via the 'global' statement) which
 *    makes certain things easier).
 * 
 *  - variables passed by reference can look like UseOfUndefinedVariable
 *    bugs but they are not. One way to fix it is to do a global analysis that
 *    remembers what are all the functions taking arguments by reference
 *    and whitelist them here. But it has a cost. One can optimize a little
 *    this by using an entity_finder computed semi lazily a la cmf multi
 *    level approach (recursive a la cpp, flib-map, git-grep, git head).
 *    Another way is to force programmers to actually declare such variables
 *    before those kinds of function calls (this is what Evan advocated).
 * 
 *  - people abuse assignements in function calls to emulate "keyword arguments"
 *    as in 'foo($color = "red", $width = 10)'. Such assignments looks
 *    like UnusedVariable but they are not. One can fix that by detecting
 *    such uses.
 * 
 *  - functions like extract(), param_get(), param_post()
 *    or variable variables like $$x introduce some false positives.
 *    Regarding the param_get/param_post(), one way to fix it is to just
 *    not analyse toplevel code. Another solution is to hardcode a few
 *    analysis that recognizes the arguments of those functions. Finally
 *    for the extract() and $$x one can just bailout of such code or
 *    as Evan did remember the first line where such code happen and
 *    don't do any analysis pass this point.
 * 
 *  - any analysis will probably flag lots of warnings on an existing PHP
 *    codebase. Some programmers may find legitimate certain things, 
 *    for instance having variables declared in a foreach to escape its
 *    foreach scope. This would then hinder the whole analysis because
 *    people would just not run the analysis. You need the approval of
 *    the PHP developers on such analysis first and get them ok to change
 *    their coding styles rules. One way to fix this problem is to have
 *    a strict mode where only certain checks are enabled. A good
 *    alternative is also to rank errors. A final trick is to report
 *    only new errors.
 * 
 * Here are some extra notes by Evan in his own variable linter:
 * 
 * "These things declare variables in a function":
 * - DONE Explicit parameters
 * - DONE Assignment (pad: variable mentionned for the first time)
 * - DONE Assignment via list()
 * - DONE Static, Global
 * - DONE foreach()
 * - DONE catch
 * - DONE Builtins ($this)
 * - DONE Lexical vars, in php 5.3 lambda expressions
 * pad: forgot pass by reference variables
 * 
 * "These things make lexical scope unknowable":
 * - DONE Use of extract()
 * - DONE Assignment or Global with variable variables ($$x)
 *   (pad: so just bailout on such code)
 * pad: forgot eval()
 * 
 * These things don't count as "using" a variable:
 * - DONE isset() (pad: this should be forbidden, it's a bad way to program)
 * - TODO empty()
 * - DONE Static class variables (pad: check done in check_classes instead)
 * 
 * Here are a few additional checks and features of this checker:
 *  - when the strict_scope flag is set, check_variables will
 *    emulate a block-scoped language as in JSLint and flags
 *    variables used outside their "block".
 *  - when passed the find_entity hook, check_variables will
 *    know about functions taking parameters by refs, which removes
 *    some false positives
 * 
 * history:
 *  - sgrimm had the simple idea of just counting the number of occurences
 *    of a variable in a program, at the token level. If only 1, then
 *    probably a typo. But sometimes variable names are mentionned in
 *    interface signatures in which case they occur only once. So you need
 *    some basic analysis; the token level is not enough. You may not
 *    need the CFG but at least you need the AST to differentiate the 
 *    different kinds of unused variables. 
 *  - Some of the scoping logic was previously in another file, scoping_php.ml
 *    but we were kind of duplicating the logic that is in scoping_php.ml.
 *    PHP has bad scoping rule allowing variables declared through a foreach
 *    to be used outside the foreach, which is probably wrong.
 *    Unfortunately, knowing from scoping_php.ml just the status of a
 *    variable (local, global, or param) is not good enough to find bugs 
 *    related to those weird scoping rules. So I've put all variable scope
 *    related stuff in this file and removed the duplication in scoping_php.ml.
 *  - I was using ast_php.ml and a visitor approach but then I rewrote it
 *    to use ast_php_simple and an "env" approach because the code was
 *    getting ugly and was containing false positives that were hard to fix.
 *    As a side effect of the refactoring, some bugs disappeared (TODO nested
 *    assigns in if, nested list(), undefined access to an array), and
 *    code regarding lexical variables became more clear because localized
 *    in one place.
 * 
 * TODO LATEST:
 * "These things declare variables in a function":
 * - Static, Global
 * These things don't count as "using" a variable:
 * - empty()
 * 
 * TODO OTHER:
 *  - nested assign in if, should work now? no more FPs?
 *  - bhiller check on array field access and unset array field
 * 
 *  - the old checker was handling correctly globals? was it looking up
 *    in the top scope? add some unit tests.
 *  - put back strict block scope
 *  - annotate Var in Ast_php
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)
type env = {
  (* The ref in the tuple is to remember the number of uses of the variable,
   * for the UnusedVariable check.
   * The ref for the Map.t is to avoid threading the env, because
   * any stmt/expression can introduce new variables.
   * 
   * todo? use a list of Map.t to represent nested scopes?
   * (globals, methods/functions, nested blocks)? when in strict/block mode?
   *)
  vars: (string, (Ast_php.tok * Scope_code.scope * int ref)) Map_poly.t ref;

  (* todo: have a globals:? *)

  (* we need to access the definitions of functions/methods to know
   * if an argument was passed by reference, in which case what looks
   * like a UseOfUndefinedVariable is actually not (but it would be
   * better for them to fix the code to introduce/declare this variable 
   * before ...).
   *)
  db: Entity_php.entity_finder option;
  (* when analyze $this->method_call(), we need to know the enclosing class *)
  in_class: name option;

  (* for better error message when the variable was inside a lambda *)
  in_lambda: bool;
  
  (* when the body of a function contains eval/extract/... we bailout
   * because we don't want to report false positives
   *)
  bailout: bool ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unused_ok s =     
  s =~ "_.*" ||
  s =~ "ignore.*" ||
  List.mem s ["unused";"dummy";"guard"] ||
  (if !E.strict
   then false
   else
    List.mem s [
      "res"; "retval"; "success"; "is_error"; "rs"; "ret";
      "e"; "ex"; "exn"; (* exception *)
    ]
  )

let lookup_opt s vars =
  Common.optionise (fun () -> Map_poly.find s vars)

let s_tok_of_name name =
  A.str_of_name name, A.tok_of_name name

(*****************************************************************************)
(* Vars passed by ref *)
(*****************************************************************************)
(* 
 * Detecting variables passed by reference is complicated in PHP because
 * one does not have to use &$var at the call site (one can though). This is
 * ugly. So to detect variables passed by reference, we need to look at
 * the definition of the function/method called, hence the need for a
 * entity_finder in env.db.
 * 
 * note that it currently returns a Ast_php.func_def, not 
 * Ast_php_simple.func_def because the database currently
 * stores concrete ASTs, not simple ASTs.
 *)
let funcdef_of_call_or_new_opt env e =
  match env.db with
  | None -> None
  | Some find_entity ->
      (match e with
      | Id name ->
          (* simple function call *)
          if not (A.is_variable name)
          then
            let s = A.str_of_name name in
            (match find_entity (Ent.Function, s) with
            | [Ast_php.FunctionE def] -> Some def
                (* nothing or multi, not our problem here *)
            | _ -> None
            )
          (* dynamic function call *)
          else None
              
      (* static method call *)
      | Class_get (Id name1, Id name2) 
          when not (A.is_variable name1) && not (A.is_variable name2) ->
          (* todo: name1 can be self/parent in traits, or static: *)
          let aclass = A.str_of_name name1 in
          let amethod = A.str_of_name name2 in
          (try
              Some (Class_php.lookup_method ~case_insensitive:true
                       (aclass, amethod) find_entity)
              (* could not find the method, this is bad, but
               * it's not our business here; this error will
               * be reported anyway in check_functions_php.ml anyway
               *)
            with 
            | Not_found | Multi_found 
            | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ ->
                None
          )
      (* simple object call. If 'this->...()' then we can use lookup_method.
       * Being complete and handling any method calls like $o->...()
       * requires to know what is the type of $o which is quite
       * complicated ... so let's skip that for now.
       * 
       * todo: special case also id(new ...)-> ?
       *)
      | Obj_get (This _, Id name2)
          when not (A.is_variable name2) ->
          (match env.in_class with
          | Some name1 ->
              let aclass = A.str_of_name name1 in
              let amethod = A.str_of_name name2 in
              (try 
                Some (Class_php.lookup_method ~case_insensitive:true
                    (aclass, amethod) find_entity)
               with 
               | Not_found | Multi_found
               | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ ->
                   None
              )
          (* wtf? use of $this outside a class? *)
          | None -> None
          )

      (* dynamic call, not much we can do ... *)
      | _ -> None
      )
      
(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(* todo: also adjust the correspoding scope_ref of name in ast_php *)
let check_defined env name ~incr_count =
  let s = A.str_of_name name in
  match lookup_opt s !(env.vars) with
  | None ->
      (* todo? could still issue an error but with the information that
       * there was an extract/eval/... around?
       *)
      if !(env.bailout)
      then ()
      else
        let err = 
          if env.in_lambda
          then E.UseOfUndefinedVariableInLambda s
          (* todo: suggest *)
          else E.UseOfUndefinedVariable (s, None)
        in
        E.fatal (A.tok_of_name name) err

  | Some (_tok, scope, access_count) ->
      if incr_count then incr access_count

let check_used env vars =
  vars +> Map_poly.iter (fun s (tok, scope, aref) ->
    if !aref = 0
    then
      if unused_ok s
      then ()
      else 
        (* if you use compact(), some variables may look unused but
         * they can actually be used. See variables_fp.php.
         *)
        if !(env.bailout)
        then ()
        else E.fatal tok (E.UnusedVariable (s, scope))
  )

let create_new_local_if_necessary ~incr_count env name =
  assert (A.is_variable name);
  let (s, tok) = s_tok_of_name name in
  match lookup_opt s !(env.vars) with
  (* new local variable implicit declaration.
   * todo: add in which nested scope? I would argue to add it
   * only in the current nested scope. If someone wants to use a
   * var outside the block, he should have initialized the var
   * in the outer context. Jslint does the same.
   *)
  | None ->
      env.vars := Map_poly.add s (tok, S.Local, ref 0) !(env.vars)
  | Some (_tok, scope, access_count) ->
      if incr_count then incr access_count
  

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* For each introduced variable (parameter, foreach variable, exception, etc), 
 * we add the binding in the environment with a counter, a la checkModule.
 * We then check at use-time if something was declared before. We then
 * finally check when we exit a scope that all variables were actually used.
 *)
let rec program env prog =
  List.iter (stmt env) prog;

  (* we must check if people used the variables declared at the toplevel
   * context or via the param_post/param_get calls.
   * todo: check env.globals instead?
   *)
  check_used env !(env.vars)

(* ---------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ---------------------------------------------------------------------- *)
and func_def env def =

  (* should not contain variables anyway, but does not hurt to check *)
  def.f_params +> List.iter (fun p -> Common.opt (expr env) p.p_default);
  
  let access_cnt = 
    match def.f_kind with
    | Function -> 0
    (* Don't report UnusedParameter for parameters of methods;
     * people sometimes override a method and don't use all
     * the parameters, hence the 1 value below.
     * 
     * less: we we don't want parameters just in method interface
     *  to not be counted as unused Parameter 
     * less: one day we will have an @override annotation in which
     *  case we can reconsider the above design decision.
     *)
    | Method _ -> 1 
  in
  let oldvars = !(env.vars) in

  let env = { env with
    (* fresh new scope, PHP has function scope (not block scope) *)
    vars = ref (def.f_params +> List.map (fun p ->
        let (s, tok) = s_tok_of_name p.p_name in
        s, (tok, S.Param, ref access_cnt)
      ) +> Map_poly.of_list
    );
    (* reinitialize bailout for each function/method *)
    bailout = ref false;
  }
  in
  def.l_uses +> List.iter (fun (is_ref, name) ->
    let (s, tok) = s_tok_of_name name in
    check_defined ~incr_count:true { env with vars = ref oldvars} name;
    (* don't reuse same access count reference; the variable has to be used
     * again in this new scope.
     *)
    env.vars := Map_poly.add s (tok, S.Closed, ref 0) !(env.vars);
  );
  (* We put 1 as 'use_count' below because we are not interested
   * in error message related to $this. It's ok to not use $this
   * in a method.
   *)
  if def.f_kind = Method && not (A.is_static def.m_modifiers)
  then begin
     let tok = (Ast_php.fakeInfo "$this") in
     env.vars := Map_poly.add "$this" (tok, S.Class, ref 1) !(env.vars);
  end;

  List.iter (stmt env) def.f_body;
  check_used env !(env.vars)

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | FuncDef def -> func_def env def
  | ClassDef def -> class_def env def
  | ConstantDef def -> constant_def env def

  | Expr e -> expr env e
  (* todo: block scope checking when in strict mode? *)
  | Block xs -> stmtl env xs

  | If (e, st1, st2) ->
      expr env e;
      stmtl env [st1;st2]

  | Switch (e, xs) ->
      expr env e;
      casel env xs

  | While (e, xs) -> 
      expr env e;
      stmtl env xs
  | Do (xs, e) ->
      stmtl env xs;
      expr env e
  | For (es1, es2, es3, xs) ->
      exprl env (es1 ++ es2 ++ es3);
      stmtl env xs
 
  | Foreach (e1, e2, e3opt, xs) ->
      expr env e1;

      (match e2 with
      (* todo: could be a Ref (Id ...) too? *)
      | Id name ->
          assert (A.is_variable name);
          let (s, tok) = s_tok_of_name name in
          (* People often use only one of the iterator when
           * they do foreach like   foreach(... as $k => $v).
           * We want to make sure that at least one of 
           * the iterator variables is used, hence this trick to
           * make them share the same access count reference.
           *)
          let shared_ref = ref 0 in

          (* todo: if already in scope? shadowing? *)
          (* todo: if strict then introduce new scope here *)
          (* todo: scope_ref := S.LocalIterator; *)

          env.vars := Map_poly.add s (tok, S.LocalIterator, shared_ref) 
            !(env.vars);

          (match e3opt with
          | None -> ()
          | Some e3 ->
              (match e3 with
              | Id name ->
                  assert (A.is_variable name);
                  let (s, tok) = s_tok_of_name name in
                  (* todo: scope_ref := S.LocalIterator; *)
                  env.vars := Map_poly.add s (tok, S.LocalIterator, shared_ref) 
                    !(env.vars);
              (* todo: E.warning tok E.WeirdForeachNoIteratorVar *)
              | _ -> raise Todo          
              )
          );
          stmtl env xs
      (* todo: E.warning tok E.WeirdForeachNoIteratorVar *)
      | _ -> raise Todo          
      );

  | Return (_, eopt)   
  | Break eopt | Continue eopt ->
      Common.opt (expr env) eopt

  | Throw e -> expr env e
  | Try (xs, c1, cs) ->
      stmtl env xs;
      catches env (c1::cs)

  | StaticVars xs ->
      xs +> List.iter (fun (name, eopt) ->
        Common.opt (expr env) eopt;
        (* todo: add in vars *)
      )
  | Global xs ->
      xs +> List.iter (fun e ->
        (* todo: should be a Id most of the time *)
        ()
      )

(* The scope of catch is actually also at the function level in PHP ...
 *
 * todo: but for this one it is so ugly that I introduce a new scope
 * even outside strict mode. It's just too ugly.
 * todo: check unused
 * todo? could use local ? could have a UnusedExceptionParameter ? 
 *)
and catch env (hint_type, name, xs) =
  let (s, tok) = s_tok_of_name name in
  env.vars := Map_poly.add s (tok, S.LocalExn, ref 0) !(env.vars);
  stmtl env xs

and case env = function
  | Case (e, xs) ->
      expr env e;
      stmtl env xs
  | Default xs ->
      stmtl env xs

and stmtl env xs = List.iter (stmt env) xs
and casel env xs = List.iter (case env) xs
and catches env xs = List.iter (catch env) xs

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  | Int _ | Double _ | String _ -> ()

  | Id name when A.is_variable name ->
      check_defined ~incr_count:true env name

  | Id name -> ()

  (* lvalue.
   * todo: factorize code with vars/array-field when passed by ref,
   * have a lvalue function?
   *)
  | Assign (None, e1, e2) ->
      (match e1 with
      | Id name ->
          (* Does an assignation counts as a use? If you only 
           * assign and never use a variable what is the point? 
           * This should be legal only for parameters (note that here
           * I talk about parameters, not arguments) passed by reference.
           *)
          create_new_local_if_necessary ~incr_count:false env name;

      (* extract all vars, and share the same reference *)
      | List xs ->
          let all_vars = xs +> List.map (function
            | Id name when A.is_variable name -> name
            | _ -> raise Todo
          )
          in
          (* Use the same trick than for LocalIterator *)
          let shared_ref = ref 0 in
          (* todo: use create_new_local_if_necessary 
           * if the variable was already existing, then 
           * better not to add a new binding cos this will mask
           * a previous one which will never get its ref 
           * incremented.
           *)
          all_vars +> List.iter (fun name ->
            let (s, tok) = s_tok_of_name name in
            env.vars := Map_poly.add s (tok, S.ListBinded, shared_ref) 
              !(env.vars);
          )

      (* todo: for bhiller *)
      | Array_get (_, e_arr, e_opt) ->
          expr env e_arr;
          Common.opt (expr env) e_opt

      (* checks for use of undefined member should be in check_classes *)
      | Obj_get (_, _) | Class_get (_, _) ->
          (* just recurse on the whole thing so the code for Obj_get/Class_get
           * below will be triggered
           *)
          expr env e1

      | _ -> raise Todo
      );
      expr env e2

  | Assign (Some _, e1, e2) ->
      exprl env [e1;e2]
  | List xs ->
      failwith "list(...) should be used only in an Assign context"

  (* A mention of a variable in a unset() should not be really
   * considered as a use of variable. There should be another
   * statement in the function that actually use the variable.
   *)
  | Call (Id ("__builtin__unset", tok), args) ->
      (match args with
      (* less: The use of 'unset' on a variable is still not clear to me. *)
      | [Id name] ->
          assert (A.is_variable name);
          check_defined ~incr_count:false env name
      (* unsetting a field *)
      | [Array_get (_, e_arr, e_opt)] ->
          raise Todo
      | _ -> raise Todo
      )
  (* special case, could factorize maybe with pass_var_by_ref *)
  | Call (Id ("sscanf", tok), x::y::vars) ->
      (* what if no x and y? wrong number of arguments, not our business here*)
      expr env x;
      expr env y;
      vars +> List.iter (function
      | Id name when A.is_variable name ->
          create_new_local_if_necessary ~incr_count:false env name
      (* less: wrong, it should be a variable? *)
      | e -> expr env e
      )

  (* the right fix is to forbid people to use isset ... *)
  | Call (Id ("__builtin__isset", tok), [Id (name)]) when A.is_variable name ->
      ()

  | Call (Id (("__builtin__eval" | "__builtin__eval_var" | 
               "extract" | "compact"
       ), _), _args) ->
      env.bailout := true;
      (* todo? else display an error? weird argument to param_xxx func? *)

      (* facebook specific? should be a hook instead to visit_prog? *)
  | Call(Id("param_post"|"param_get"|"param_request"|"param_cookie"as kind,tok),
        (ConsArray (_, _, array_args))::rest_param_xxx_args) ->

      (* have passed a 'prefix' arg, or nothing *)
      if List.length rest_param_xxx_args <= 1
      then begin
        let prefix_opt =
          match rest_param_xxx_args with
          | [String(str_prefix, _tok_prefix)] -> 
              Some str_prefix
          | [] ->
              (match kind with
              | "param_post" -> Some "post_"
              | "param_get" -> Some "get_"
              | "param_request" -> Some "req_"
              | "param_cookie" -> Some "cookie_"
              | _ -> raise Impossible
              )
          | _ -> 
              (* less: display an error? weird argument to param_xxx func?*)
              None
        in
        prefix_opt +> Common.do_option (fun prefix ->
          array_args +> List.iter (function
          | Akval(String(param_string, tok_param), _typ_param) ->
              let s = "$" ^ prefix ^ param_string in
              let tok = A.tok_of_name (param_string, tok_param) in
              env.vars := Map_poly.add s (tok, S.Local, ref 0) !(env.vars);
              (* less: display an error? weird argument to param_xxx func? *)
          | _ -> ()
          )
        )
      end

  | Call (e, es) ->
      expr env e;
      
      (* getting the def for args passed by ref false positives fix *)
      let def_opt = funcdef_of_call_or_new_opt env e in
      let es_with_parameters =
        match def_opt with
        | None -> 
            es +> List.map (fun e -> e, None)
        | Some def ->
            let params = 
              def.Ast_php.f_params +> Ast_php.unparen +> Ast_php.uncomma_dots
            in
            (* maybe the #args does not match #params, but this is not our
             * business here; this will be detected anyway in check_functions
             * or check_classes.
             *)
            Common.zip_safe es (List.map (fun p -> Some p) params)
      in

      es_with_parameters +> List.iter (fun (arg, param_opt) ->
        match arg, param_opt with
        (* keyword argument; do not consider this variable as unused.
         * We consider this variable as a pure comment here and just pass over.
         * todo: could make sure they are not defined in the current
         * environment in strict mode? and if they are, shout because of
         * bad practice?
         *)
        | Assign (None, Id name, e2), _ ->
            expr env e2
        (* a variable passed by reference, this can considered a new decl *)
        | Id name, Some {Ast_php.p_ref = Some _;_} when A.is_variable name ->

            (* if was already assigned and passed by refs, 
             * increment its use counter then.
             * less: or should we increase only if inout param?
             *)
            create_new_local_if_necessary ~incr_count:true env name

        | _ -> expr env arg
      )


  | This name ->
      (* when we do use($this) in closures, we create a fresh $this variable
       * with a refcount of 0, so we need to increment it here.
       *)
      check_defined ~incr_count:true env name

  (* array used as an rvalue; the lvalue case should be handled in Assign. *)
  | Array_get (_, e, eopt) ->
      expr env e;
      Common.opt (expr env) eopt

  | Obj_get (e1, e2) | Class_get (e1, e2) ->
      expr env e1;
      (match e2 with
      (* with 'echo A::$v' we should not issue a UseOfUndefinedVariable,
       * check_classes_php.ml will handle this case.
       *)
      | Id _ -> ()
      | _ -> expr env e2
      )

  (* transform in code with Call to factorize code *)
  | New (e, es) ->
      expr env (Call (Class_get(e, Id ("__construct", None)), es))

  | InstanceOf (e1, e2) -> exprl env [e1;e2]

  | Infix (_, e) | Postfix (_, e) | Unop (_, e) -> expr env e
  | Binop (_, e1, e2) -> exprl env [e1; e2]
  | Guil xs -> exprl env xs

  | Ref e -> expr env e

  | ConsArray (_, _, xs) -> array_valuel env xs
  | Xhp x -> xml env x

  | CondExpr (e1, e2, e3) -> exprl env [e1; e2; e3]
  | Cast (_, e) -> expr env e

  | Lambda def ->
      func_def { env with in_lambda = true } def

and array_value env = function
  | Aval e -> expr env e
  | Akval (e1, e2) -> exprl env [e1; e2]  

and xml env x =
  raise Todo

and exprl env xs = List.iter (expr env) xs
and array_valuel env xs = List.iter (array_value env) xs

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(* checks for use of undefined members should be in check_classes, but
 * it's hard to do locally.
 * 
 * todo: inline the code of traits?
 *)
and class_def env def =
  let env = { env with in_class = Some def.c_name } in
  List.iter (constant_def env) def.c_constants;
  List.iter (class_var env) def.c_variables;
  List.iter (method_def env) def.c_methods

(* cst_body should be a static scalar so there should not be any
 * variable in it so in theory we don't need to check it ... doesn't
 * hurt though, one day maybe this will change.
 *)
and constant_def env def =
  expr env def.cst_body

and class_var env v =
  Common.opt (expr env) v.cv_value

and method_def env x = func_def env x

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_and_annotate_program2 find_entity prog =
  let env = {
    vars = ref Map_poly.empty;
    (* todo?
       [Env_php.globals_builtins +> List.map (fun s ->
       fake_var s, (S.Global, ref 1)
       )];
    *)
    db = find_entity;
    (* todo: extract all vars and their scope_ref and keep that in a 
     * symbol table so one can find them back.
     *)
    in_class = None;
    in_lambda = false;
    bailout = ref false;
  }
  in
  let ast = Ast_php_simple_build.program_with_position_information prog in
  let _env = program env ast in
  ()

let check_and_annotate_program a b = 
  Common.profile_code "Checker.variables" (fun () -> 
    check_and_annotate_program2 a b)
