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
 *)
type error = {
  typ: error_kind;
  loc: Ast_php.info;
  sev: severity;
}
 (* todo? Advise | Noisy | Meticulous ? *)
 and severity = Fatal | Warning

(* coupling: if you add a constructor here, don't forget to extend
 * layer_checker_php.ml too.
 *)
 and error_kind = 
  (* entities *)
  | UndefinedEntity of Entity_php.id_kind * Ast_php.name
  | MultiDefinedEntity of Entity_php.id_kind * Ast_php.name *
      (Ast_php.name * Ast_php.name)

  (* call sites *)
  | TooManyArguments   of name (* def *)
  | NotEnoughArguments of name (* def *)
  | WrongKeywordArgument of Ast_php.dname * Ast_php.parameter * severity2

  (* variables *)
  | UseOfUndefinedVariable of Ast_php.dname
  | UnusedVariable of Ast_php.dname * Scope_php.phpscope

  (* classes *)
  | UseOfUndefinedMember of Ast_php.name

  (* bail-out constructs *)
  | UglyGlobalDynamic
  | WeirdForeachNoIteratorVar

  (* cfg, mostly DeadCode statements *)
  | CfgError of Controlflow_build_php.error_kind
  | CfgPilError of Controlflow_build_pil.error_kind

  and severity2 =
   | Bad
   | ReallyBad
   | ReallyReallyBad

exception Error of error

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_severity2 = function
  | Bad -> "Bad" 
  | ReallyBad -> "ReallyBad"
  | ReallyReallyBad -> "ReallyReallyBad"

let string_of_error_kind error_kind =
  match error_kind with
  | UndefinedEntity(kind, name) ->
      spf "Undefined entity %s %s"
        (Entity_php.string_of_id_kind kind)
        (Ast.name name)

  | MultiDefinedEntity(kind, name, (ex1, ex2)) ->
      spf "Multiply defined entity %s %s"
        (Entity_php.string_of_id_kind kind)
        (Ast.name name)
     (* todo? one was declared: %s and the other %s    or use tbgs ... *)

  | TooManyArguments defname ->
      "Too many arguments"
     (* todo? function was declared: %s     or use tbgs ... *)
  | NotEnoughArguments defname ->
      "Not enough arguments"
     (* todo? function was declared: %s    or use tbgs *)
  | WrongKeywordArgument(dn, param, severity) ->
      spf "Wrong keyword argument, %s <> %s (%s)"
        (Ast.dname dn) (Ast.dname param.p_name) (string_of_severity2 severity)

  | UseOfUndefinedVariable (dname) ->
      let s = Ast.dname dname in
      spf "Use of undeclared variable $%s. " s ^
"Declare variables prior to use (even if you are passing them as reference
    parameters). You may have misspelled this variable name.
"
  | UnusedVariable (dname, scope) ->
      let s = Ast.dname dname in
      spf "Unused %s variable $%s" (Scope_php.s_of_phpscope scope) s 

  | UseOfUndefinedMember (name) ->
      let s = Ast.name name in
      spf "Use of undefined member $%s" s

  | UglyGlobalDynamic ->
      "Ugly dynamic global declaration"
  | WeirdForeachNoIteratorVar ->
      "Weird, foreach with not a var as iterator"

  | CfgError err ->
      Controlflow_build_php.string_of_error_kind err
  | CfgPilError err ->
      Controlflow_build_pil.string_of_error_kind err

(* note that the output is emacs compile-mode compliant *)
let string_of_error error =
  (* todo? use severity? *)
  let info = Ast.parse_info_of_info error.loc in
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
(* Ranking bis *)
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
        let s = Ast.dname dname in
        hcount_str#update s (fun old -> old+1);
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

let (find_entity:
  find_entity: Entity_php.entity_finder option ->
  (Entity_php.id_kind * Ast_php.name) ->
  Ast_php.entity option) = 
 fun ~find_entity (kind, name) ->

  match find_entity with
  | None -> None
  | Some find_entity ->
      let str = Ast.name name in
      let ids_ast = find_entity (kind, str) in
      (match ids_ast with
      | [x] -> Some x
      | [] ->
          fatal (Ast.info_of_name name) (UndefinedEntity (kind, name));
          None
      | x::y::xs ->
          if Hashtbl.mem h_already_error (kind, str) 
          then ()
          else begin
            Hashtbl.replace h_already_error (kind, str) true;
            (* todo: to give 2 ex of defs *)
            let ex1 = name (* TODO *) in
            let ex2 = name (* TODO *) in
            fatal (Ast.info_of_name name) 
              (MultiDefinedEntity (kind, name, (ex1, ex2)));
          end;
          (* can give the first one ... *)
          Some x
      )
