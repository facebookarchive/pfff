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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Centralize PHP errors report functions (they did the same in c--)
 * 
 * TODO: move more of the code of lint_php.mli here
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let strict = ref false

(* see also _errors below *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* I used to have just type error = error_kind, but then I would need a
 * info_of_error() function to extract the position for each error. It 
 * is simpler to put the error location in a field and have a simple
 * error_kind (having a simple error_kind independent of location also
 * makes it easy to have "stable" errors independent of repository
 * which makes it easy to have cmf --only-new-errors working correctly).
 * 
 *)
type error = {
  typ: error_kind;
  loc: Ast_php.info;
  (* less: maybe severity should be inferred from the error_kind. That
   * way it will also avoid the need for 2 functions fatal()/warning()
   * and just have error().
   *)
  sev: severity; 
}
 (* less: Advice | Noisy | Meticulous/Pedantic ? *)
 and severity = Fatal | Warning

(* Mostly use/def kind of errors
 * (for functions/classes/constants, variables, members, files, etc)
 * 
 * coupling: if you add a constructor here, don't forget to extend
 * layer_checker_php.ml too.
 * 
 * note: try to not put structure that have position information in 
 * the type below (so use string, not Ast_php.name), so that the
 * error_kind is location independent and can be used portably as a key
 * through different repository (cf cmf --only-new-errors).
 *)
 and error_kind = 
  (* entities *)
  | UndefinedEntity of Entity_php.id_kind * string (* name *)
  | MultiDefinedEntity of Entity_php.id_kind * string (* name *) *
      (string * string) (* name * name *)
  | UndefinedClassWhileLookup of string
  | UndefinedMethodInAbstractClass of string

  (* call sites *)
  | TooManyArguments   of string (* name *) (* def *)
  | NotEnoughArguments of string (* name *) (* def *)
  | WrongKeywordArgument of (* erling's idea *)
      string (* dname *) * string (* parameter *) * severity2
  | CallingStaticMethodWithoutQualifier of string
  | CallingMethodWithQualifier of string
  | PassingUnexpectedRef (* alok's idea *)

  (* variables *)
  | UseOfUndefinedVariable of string (* dname *) * suggest option
  | UnusedVariable of string (* dname *) * Scope_php.phpscope
  | UseOfUndefinedVariableInLambda of string (* dname *)

  (* classes (could be put in UndefinedEntity (ClassMember)) *)
  | UseOfUndefinedMember of string (* name *) * suggest option

  (* wrong include/require *)
  | FileNotFound of Common.filename

  (* bail-out constructs *)
  | UglyGlobalDynamic
  | WeirdForeachNoIteratorVar

  (* cfg, mostly DeadCode statements *)
  | CfgError of Controlflow_build_php.error_kind
(*  | CfgPilError of Controlflow_build_pil.error_kind *)


  (* tainting *)
  | Injection of injection_kind (* todo: * explanation (e.g. a path?) *)

  (* some code is using 'case 1;' instead of 'case 1:', ugly *)
  | CaseWithSemiColon
  (* php is case insensitive but it's better to have consistent code
   * that always use the lowercase version of a keyword
   *)
  | CaseSensitivityKeyword

  (* todo: 
   *  - type errors, 
   *  - protocol errors (statistical analysis), 
   *  - etc 
   *)

  and severity2 =
   | Bad
   | ReallyBad
   | ReallyReallyBad
  and suggest = string * int (* edit distance *)
  and injection_kind = XSS | Sql | Shell

exception Error of error

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_severity2 = function
  | Bad -> "Bad" 
  | ReallyBad -> "ReallyBad"
  | ReallyReallyBad -> "ReallyReallyBad"

let string_of_suggest_opt x =
  match x with
  | None -> ""
  | Some (s, _i) -> 
      spf " (did you mean %s?)" s

let string_of_error_kind error_kind =
  match error_kind with
  | UndefinedEntity(kind, name) ->
      spf "Undefined %s %s" (Entity_php.string_of_id_kind kind) name

  | MultiDefinedEntity(kind, name, (ex1, ex2)) ->
     (* todo? one was declared: %s and the other %s    or use tbgs ... *)
      spf "Multiply defined %s %s"(Entity_php.string_of_id_kind kind)
        name
  | UndefinedClassWhileLookup (name) ->
      spf "Undefined class while lookup inheritance tree: %s" name
  | UndefinedMethodInAbstractClass (name) ->
      spf "Undefined method in abstract class: %s" name

  | TooManyArguments defname ->
     (* todo? function was declared: %s     or use tbgs ... *)
      "Too many arguments"
  | NotEnoughArguments defname ->
     (* todo? function was declared: %s    or use tbgs *)
      "Not enough arguments"
  | WrongKeywordArgument(dn, param, severity) ->
      spf "Wrong keyword argument, %s <> %s (%s)"
        dn param (string_of_severity2 severity)
  | CallingStaticMethodWithoutQualifier name ->
      spf "Calling static method %s without a qualifier" name
  | CallingMethodWithQualifier name ->
      spf "Calling non static method %s with a qualifier" name
  | PassingUnexpectedRef ->
      "passing a reference to a function not expecting one"

  | UseOfUndefinedVariable (dname, x) ->
      spf "Use of undeclared variable $%s%s. " dname (string_of_suggest_opt x)
(*
"Declare variables prior to use (even if you are passing them as reference
    parameters). You may have misspelled this variable name.
"
*)

  | UseOfUndefinedVariableInLambda (dname) ->
      spf "Use of undeclared variable $%s in lambda. " dname ^
"See http://php.net/manual/en/functions.anonymous.php and the 'use' keyword."

  | UnusedVariable (dname, scope) ->
      spf "Unused %s variable $%s" (Scope_php.s_of_phpscope scope) dname

  | UseOfUndefinedMember (name, x) ->
      spf "Use of undefined member $%s%s" name (string_of_suggest_opt x)

  | UglyGlobalDynamic ->
      "Ugly dynamic global declaration"
  | WeirdForeachNoIteratorVar ->
      "Weird, foreach with not a var as iterator"

  | CfgError err ->
      Controlflow_build_php.string_of_error_kind err
(*  | CfgPilError err ->
      Controlflow_build_pil.string_of_error_kind err
*)

  | FileNotFound s ->
      spf "File not found %s" s

  | Injection kind ->
      let s =
        match kind with
        | XSS -> "XSS"
        | Sql -> "Sql"
        | Shell -> "Shell"
      in
      spf "%s injection" s
  | CaseWithSemiColon ->
      "Use a colon not a semicolon"
  | CaseSensitivityKeyword ->
      "Use the lowercase version of the keyword"

(* note that the output is emacs compile-mode compliant *)
let string_of_error error =
  (* todo? use severity? *)
  let info = Parse_info.parse_info_of_info error.loc in
  spf "%s:%d:%d: CHECK: %s" 
    info.Parse_info.file info.Parse_info.line info.Parse_info.column
    (string_of_error_kind error.typ)

let report_error err = 
  pr2 (string_of_error err)

(*****************************************************************************)
(* Global bis *)
(*****************************************************************************)

(* Ugly. Common.save_excursion can help reduce the problems that can
 * come from using a global.
 *)
let _errors = ref []

(* todo? let exn_when_error *)

let fatal loc err =
  Common.push2 { loc = loc; typ = err; sev = Fatal } _errors
let warning loc err = 
  Common.push2 { loc = loc; typ = err; sev = Warning } _errors

let report_all_errors () = 
  !_errors |> List.rev |> List.iter report_error

(*****************************************************************************)
(* Ranking *)
(*****************************************************************************)

(* ranking errors, inspired by Engler slides *)
let rank_errors errs =
  errs +> List.map (fun x ->
    x,
    match x.typ with
    | UnusedVariable (_, Scope_code.Local) -> 10
    | CfgError (Controlflow_build_php.DeadCode node_kind) ->
        (match node_kind with
        | Controlflow_php.Break -> 3
        | Controlflow_php.Return -> 3
        | _ -> 15
        )
    | CfgError _ -> 11
    | UseOfUndefinedMember _ -> 5
    | _ -> 0
  ) +> Common.sort_by_val_highfirst +> Common.map fst


let show_10_most_recurring_unused_variable_names () =

  (* most recurring probably false positif *)
  let hcount_str = Common.hash_with_default (fun() -> 0) in

  !_errors +> List.iter (fun err ->
    match err.typ with
    | UnusedVariable (dname, scope) ->
        hcount_str#update dname (fun old -> old+1);
    | _ -> ()
  );
  pr2 "top 10 most recurring unused variable names";
  hcount_str#to_list +> Common.sort_by_val_highfirst +> Common.take_safe 10
   +> List.iter (fun (s, cnt) ->
        pr2 (spf " %s -> %d" s cnt)
      );
  ()

let filter_false_positives err = 
  err +> Common.exclude (fun x ->
    match x.typ with
    (* this actually requires a global analysis to truly know if the class
     * variable is unused *)
    | UnusedVariable (_, Scope_code.Class) -> true
    | _ -> false
  )

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let (h_already_error: ((Entity_php.id_kind * string, bool) Hashtbl.t)) = 
  Hashtbl.create 101 

let (find_entity_and_warn:
  Entity_php.entity_finder -> (Entity_php.id_kind * Ast_php.name) ->
  (Ast_php.entity -> unit) -> unit) =
 fun find_entity (kind, name) callback ->

   let str = Ast.name name in
   let ids_ast = find_entity (kind, str) in
   match ids_ast with
   | [x] -> callback x
   | [] ->
       fatal (Ast.info_of_name name) (UndefinedEntity (kind, str));
       
   | x::y::xs ->
       if Hashtbl.mem h_already_error (kind, str) 
       then ()
       else begin
         Hashtbl.add h_already_error (kind, str) true;
         (* todo: to give 2 ex of defs *)
         let ex1 = str (* TODO *) in
         let ex2 = str (* TODO *) in
         fatal (Ast.info_of_name name) 
           (MultiDefinedEntity (kind, str, (ex1, ex2)));
       end;
       (* can give the first one ... *)
       callback x
   
