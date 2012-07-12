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
module Ent = Entity_php

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
 * 
 * This file is concerned with variables, that is Ast_php.dname
 * entities, so for completness C-s for dname in ast_php.ml and
 * see if all uses of it are covered. Other files are more concerned
 * about checks related to entities, that is Ast_php.name.
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
 *    their coding styles rules. A good alternative is to rank errors.
 * 
 *  -  Another issue is the implicitly-declared-when-used-the-first-time
 *     ugly semantic of PHP. it's ok to do  if(!($a = foo())) { foo($a) }
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
 * 
 * "These things make lexical scope unknowable":
 * - DONE Use of extract()
 * - SEMI Assignment or Global with variable variables ($$x)
 *   (pad: I actually bail out on such code)
 * 
 * These things don't count as "using" a variable:
 * - DONE isset() (pad: this should be forbidden, it's a bad way to program)
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
 * 
 * history:
 *  - sgrimm had the simple idea of just counting the number of occurences
 *    of variables in a program, at the token level. If only 1, then
 *    probably a typo. But sometimes variable names are mentionned in
 *    interface signatures in which case they occur only once. So you need
 *    some basic analysis; the token level is not enough. You may not
 *    need the CFG but at least you need the AST to differentiate the 
 *    different kinds of unused variables. 
 *  - Some of the scoping logic was previously in another file, scoping_php.ml
 *    But we were kind of duplicating the logic that is in scoping_php.ml.
 *    PHP has bad scoping rule allowing variables declared through a foreach
 *    to be used outside the foreach, which is probably wrong.
 *    Unfortunately, knowing from scoping_php.ml just the status of a
 *    variable (local, global, or param) is not good enough to find bugs 
 *    related to those weird scoping rules. So I've put all variable scope
 *    related stuff in this file and removed the duplication in scoping_php.ml.
 *  - I was using ast_php.ml but then I rewrote it to use ast_php_simple
 *    because the code was getting ugly and was containing false
 *    positives that were hard to fix.
 * 
 * TODO LATEST:
 * "These things declare variables in a function":
 * - Static, Global
 * - foreach()
 * - catch
 * - Builtins ($this)
 * - Lexical vars, in php 5.3 lambda expressions
 * - Assignment via list()
 * 
 * "These things make lexical scope unknowable":
 * - Use of extract()
 * - Assignment or Global with variable variables ($$x)
 *   (pad: I actually bail out on such code)
 * 
 * These things don't count as "using" a variable:
 * - isset() (pad: this should be forbidden, it's a bad way to program)
 * - empty()
 * - Static class variables
 * 
 *  - param_post, param_get
 *  - list assign
 *  - bailout eval, extract, etc
 *  - keyword arguments
 *  - lambda special, handle use too
 *  - passed by ref
 *  - isset, unset
 *  - this
 *  - globals
 * 
 *  - nested assign in if, should work now? no more FPs?
 *  - bhiller check on array field access and unset array field
 * 
 *  - the old checker was handling correctly globals? was it looking up
 *    in the top scope? add some unit tests.
 *  - put back strict block scope
 *  - annotate Var in Ast_php
 * 
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)
type env = {
  (* todo? use a list of SMap.t to represent nested scopes?
   * (globals, methods/functions, nested blocks)? when in strict/block mode?
   * 
   * The ref in the tuple is to record the number of uses of the variable,
   * for the UnusedVariable check.
   * The ref for the SMap is to avoid threading the env, because
   * any stmt/expression can introduce new variables.
   *)
  vars: (string, (Ast_php.tok * Scope_code.scope * int ref)) Map_poly.t ref;

  (* todo: have a globals:? *)

  (* we need to access the definitions of functions/methods to know
   * if an argument was passed by reference, in which case what looks
   * like a UseOfUndefinedVariable is actually not (but it would be
   * better for them to fix the code to introduce/declare this variable 
   * before).
   *)
  db: Entity_php.entity_finder option;
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

let fake_var s = 
  (s, None)

let lookup_opt s vars =
  Common.optionise (fun () -> Map_poly.find s vars)

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

let check_undefined name env =
  let s = A.str_of_name name in
  match lookup_opt s !(env.vars) with
  | None ->
      (* todo: bailout, lambda, suggest *)
      E.fatal (A.tok_of_name name) (E.UseOfUndefinedVariable (s, None))
  | Some (_tok, scope, access_count) ->
      incr access_count

(* less: if env.bailout? *)
let check_unused vars =
  vars +> Map_poly.iter (fun s (tok, scope, aref) ->
    if !aref = 0
    then
      if unused_ok s
      then ()
      else E.fatal tok (E.UnusedVariable (s, scope))
  )

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)

(* For each introduced variable (parameter, foreach variable, exception, etc), 
 * we add the binding in the environment with a counter, a la checkModule.
 * We then check at use time if something was declared before. We then
 * finally check when we exit a scope that all variables were actually used.
 *)
let rec program env prog =
  List.iter (stmt env) prog;
  (* todo: check env.globals instead? *)

  (* we must check if people used the variables declared at the toplevel
   * context or via the param_post/param_get calls.
   *)
  check_unused !(env.vars)

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_def env def =

  def.f_params +> List.iter (fun p -> Common.opt (expr env) p.p_default);
  
  let access_cnt = 
    match def.f_kind with
    | Function -> 0
    (* Don't report UnusedParameter for parameters of methods;
     * people sometimes override a method and don't use all
     * the parameters, hence the cnt below.
     * less: one day we will have an @override annotation in which
     * case we can reconsider the above design decision.
     *)
    | Method _ -> 1 
  in

  let env = { env with
    vars = ref (
      def.f_params +> List.map (fun p ->
        A.str_of_name p.p_name,
        (A.tok_of_name p.p_name, S.Param, ref access_cnt)
      )
      (* todo: add $this if not method non-static *) 
      +> Map_poly.of_list
    )
  }
  in
  (* todo: if lambda, then add also l_uses and increment use count?
   * or just reuse the same aref?
   *)

  List.iter (stmt env) def.f_body;
  check_unused !(env.vars)

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | FuncDef def -> func_def env def
  | Expr e -> expr env e
  | _ -> 
      ()

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  | Int _ | Double _ | String _ -> ()

  | Id name when A.is_variable name ->
      (* todo: also adjust the correspoding scope_ref of name in ast_php.
       * do that in check_undefined?
       *)
      check_undefined name env
  | Id name -> ()

  | Assign (None, e1, e2) ->
      (match e1 with
      | Id name ->
          assert (A.is_variable name);
          (* skeleton similar to check_undefined() *)
          let s = str_of_name name in
          let tok = tok_of_name name in
          (match lookup_opt s !(env.vars) with
          (* new local variable implicit declaration.
           * todo: add in which nested scope? I would argue to add it
           * only in the current nested scope. If someone wants to use a
           * var outside the block, he should have initialized the var
           * in the outer context. Jslint does the same.
           *)
          | None ->
              env.vars := Map_poly.add s (tok, S.Local, ref 0) !(env.vars)
          | Some (_tok, scope, access_count) ->
              (* Does an assignation counts as a use? If you only 
               * assign and never use a variable what is the point? 
               * This should be legal only for parameters (note that here
               * I talk about parameters, not arguments) passed by reference.
               *)
              ()
          )
      (* todo: extract all vars *)
      | List xs ->
          ()
      (* todo: for bhiller *)
      | Array_get (e_arr, e_opt) ->
          ()
      | _ -> raise Todo
      );
      expr env e2

  | Assign (Some _, e1, e2) ->
      exprl env [e1;e2]

  (* todo: keyword arguments false positives fix, intercept the Assign
   *  that is above.
   * todo: args passed by ref false positives fix
   *)
  | Call (e, es) ->
      expr env e;
      List.iter (expr env) es

  | _ -> 
      ()

and exprl env xs = List.iter (expr env) xs

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

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
  }
  in
  let ast = Ast_php_simple_build.program_with_position_information prog in
  let _env = program env ast in
  ()

let check_and_annotate_program a b = 
  Common.profile_code "Checker.variables" (fun () -> 
    check_and_annotate_program2 a b)
