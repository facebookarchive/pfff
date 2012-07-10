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
 *    the first assgignement "declares" the variable (on the other side
 *    the PHP language forces people to explicitly declared
 *    the use of global variables (via the 'global' statement) which
 *    makes certain things easier).
 * 
 *  - variables passed by reference can look like UseOfUndefinedVariable
 *    bugs but they are not. One way to fix it is to do a global analysis that
 *    remembers what are all the functions taking arguments by reference
 *    and whitelist them here. But it has a cost. One can optimize a little
 *    this by using an entity_finder computed semi lazily a la cmf mutli
 *    level approach (recursive a la cpp, flib-map, git-grep, git head).
 *    Another way is to force programmers to actually declare such variables
 *    before those kinds of function calls (this is what evan advocated).
 * 
 *  - people abuse assignements in function call to emulate "keyword arguments"
 *    as in 'foo($color = "red", $width = 10)'. Such assignements looks
 *    like UnusedVariable but they are not. One can fix that by detecting
 *    such uses.
 * 
 *  - functions like extract(), param_get(), param_post()
 *    or variable variables like $$x introduce some false positives.
 *    Regarding the param_get/param_post(), one way to fix it is to just
 *    not analyse toplevel code. Another solution is to hardcode a few
 *    analysis that recognizes the arguments of those functions. Finally
 *    for the extract() and $$x one can just bailout of such code or
 *    as evan did remember the first line where such code happen and
 *    don't do any analysis pass this point.
 * 
 *  - any analysis will probably flag lots of warnings on an existing PHP
 *    codebase. Some programmers may find legitimate certain things, 
 *    for instance having variables declared in a foreach to escape its
 *    foreach scope. This would then hinder the whole analysis because
 *    people would just not run the analysis. You need the approval of
 *    the PHP developers on such analysis first and get them ok to change
 *    their coding styles rules.
 * 
 *  -  Another issue is the implicitly-declared-when-used-the-first-time
 *     ugly semantic of PHP. it's ok to do  if(!($a = foo())) { foo($a) }
 * 
 * Here are some extra notes by Evan in his own variable linter:
 * 
 * "These things declare variables in a function":
 * - DONE Explicit parameters
 * - DONE Static, Global
 * - DONE foreach()
 * - DONE catch
 * - DONE Builtins ($this)
 * - DONE Lexical vars, in php 5.3 lambda expressions
 * - DONE Assignment via list()
 * - SEMI Assignment
 *   (pad: variable mentionned for the first time)
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
 *    interface signature in which case they occur only once. So you need
 *    some basic analysis; the token level is not enough. You may not
 *    need the CFG but at least you need the AST to differentiate the 
 *    different kinds of unused variables. 
 *  - Some of the scoping logic was previously in another file, scoping_php.ml
 *    But we were kind of duplicating the logic that is in scoping_php.ml 
 *    PHP has bad scoping rule allowing variable declared through a foreach
 *    to be used outside the foreach, which is probably wrong. 
 *    Unfortunately, knowing from scoping_php.ml just the status of a
 *    variable (local, global, or param) is not good enough to find bugs 
 *    related to those weird scoping rules. So I've put all variable scope
 *    related stuff in this file and removed the duplication in scoping_php.ml
 *  - I was using ast_php.ml but then I rewrote it to use ast_php_simple
 *    because the code was getting ugly and was containing false
 *    positives that were hard to fix.
 *)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)
type env = {
  (* We use a list of list to represent nested scopes (globals,
   * methods/functions, nested blocks).
   * The ref is for the number of uses.
   *)
  vars: (A.name * (Scope_code.scope * int ref)) list list;
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
let unused_ok_when_no_strict s = 
  if not!E.strict 
  then
    List.mem s 
      ["res"; "retval"; "success"; "is_error"; "rs"; "ret";
       "e"; "ex"; "exn"; (* exception *)
      ]
  else false

let unused_ok s =     
  s =~ "_.*" ||
  s =~ "ignore.*" ||
  List.mem s ["unused";"dummy";"guard"] ||
  unused_ok_when_no_strict s

let fake_var s = 
  (s, None)

(*****************************************************************************)
(* Checks *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* For each introduced variable (parameter, foreach variable, exception, etc), 
 * we add the binding in the environment with a counter, a la checkModule.
 * We then check at use time if something was declared before. We then
 * finally check when we exit a scope that all variables were actually used.
 *)
let rec program env prog = 
  ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check_and_annotate_program2 find_entity prog =
  let env = {
    vars = [Env_php.globals_builtins +> List.map (fun s ->
      fake_var s, (S.Global, ref 1)
    )];
    db = find_entity;
    (* todo: extract all vars and their scope_ref *)
  }
  in
  let ast = Ast_php_simple_build.program prog in
  let _env = program env ast in
  ()

let check_and_annotate_program a b = 
  Common.profile_code "Checker.variables" (fun () -> 
    check_and_annotate_program2 a b)
