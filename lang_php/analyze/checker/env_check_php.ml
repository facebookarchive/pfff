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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

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
  || s =~ "ignore.*"
  || s = "guard"
  || unused_ok_when_no_strict s

let fake_dname s = 
  DName (s, Ast.fakeInfo s)

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
