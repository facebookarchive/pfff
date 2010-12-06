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

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* the position in the name below correspond to the function at the call site 
 * coupling: if you add a constructor here, don't forget to extend
 * layer_checker_php.ml too
 *)
type error = 
  (* entities *)
  | UndefinedEntity of Entity_php.id_kind * Ast_php.name
  | MultiDefinedEntity of Entity_php.id_kind * Ast_php.name *
      (Ast_php.name * Ast_php.name)

  (* call sites *)
  | TooManyArguments   of (Ast_php.info * name (* def *))
  | NotEnoughArguments of (Ast_php.info * name (* def *))
  | WrongKeywordArgument of Ast_php.dname * Ast_php.parameter * severity

  (* variables *)
  | UseOfUndefinedVariable of Ast_php.dname
  | UnusedVariable of Ast_php.dname * Scope_php.phpscope

  (* classes *)
  | UseOfUndefinedMember of Ast_php.name

  (* bail-out constructs *)
  | UglyGlobalDynamic of Ast_php.info
  | WeirdForeachNoIteratorVar of Ast_php.info

  (* cfg, mostly DeadCode statements *)
  | CfgError of Controlflow_build_php.error
  | CfgPilError of Controlflow_build_pil.error
 and severity =
   | Bad
   | ReallyBad
   | ReallyReallyBad

exception Error of error

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_severity = function
  | Bad -> "Bad" 
  | ReallyBad -> "ReallyBad"
  | ReallyReallyBad -> "ReallyReallyBad"

let string_of_error error =
  let spos info = 
    (* emacs compile-mode compatible output *)
    spf "%s:%d:%d: " 
      info.Parse_info.file info.Parse_info.line info.Parse_info.column
  in
  match error with

  | UndefinedEntity(kind, name) ->
      let info = Ast.parse_info_of_info (Ast.info_of_name name) in
      (spos info ^ (spf "CHECK: undefined entity %s %s"
                  (Entity_php.string_of_id_kind kind)
                  (Ast.name name)))

  | MultiDefinedEntity(kind, name, (ex1, ex2)) ->
      let info = Ast.parse_info_of_info (Ast.info_of_name name) in
      (spos info ^ (spf "CHECK: multiply defined entity %s %s"
                  (Entity_php.string_of_id_kind kind)
                  (Ast.name name)))
     (* todo? one was declared: %s and the other %s    or use tbgs ... *)

  | TooManyArguments (info, defname) ->
      let info = Ast.parse_info_of_info info in
      (spos info ^ "CHECK: too many arguments");
     (* todo? function was declared: %s     or use tbgs ... *)
  | NotEnoughArguments (info, defname) ->
      let info = Ast.parse_info_of_info info in
      (spos info ^ "CHECK: not enough arguments");
     (* todo? function was declared: %s    or use tbgs *)
  | WrongKeywordArgument(dn, param, severity) ->
      let info = Ast.info_of_dname dn +> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: wrong keyword argument, %s <> %s (%s)"
        (Ast.dname dn) (Ast.dname param.p_name) (string_of_severity severity)



  | UseOfUndefinedVariable (dname) ->
      let s = Ast.dname dname in
      let info = Ast.info_of_dname dname |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: use of undefined variable $%s" s

  | UnusedVariable (dname, scope) ->
      let s = Ast.dname dname in
      let info = Ast.info_of_dname dname |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: unused %s variable $%s" 
              (Scope_php.s_of_phpscope scope)
              s 

  | UseOfUndefinedMember (name) ->
      let s = Ast.name name in
      let info = Ast.info_of_name name |> Ast.parse_info_of_info in
      spos info ^ spf "CHECK: use of undefined member $%s" s



  | UglyGlobalDynamic info ->
      let pinfo = Ast.parse_info_of_info info in
      spos pinfo ^ "CHECK: ugly dynamic global declaration"
  | WeirdForeachNoIteratorVar info ->
      let pinfo = Ast.parse_info_of_info info in
      spos pinfo ^ "CHECK: weird, foreach with not a var as iterator"

  | CfgError err ->
      Controlflow_build_php.string_of_error err
  | CfgPilError err ->
      Controlflow_build_pil.string_of_error err

 
        
let info_of_error err =
  match err with
  | UndefinedEntity (_, name)
  | MultiDefinedEntity (_, name, (_, _))
      -> Some (Ast.info_of_name name)

  | TooManyArguments  (call_info, defname) ->
      Some call_info
  | NotEnoughArguments (call_info, defname) ->
      Some call_info
  | WrongKeywordArgument (dname,  param, _) ->
      Some (Ast.info_of_dname dname)

  | UseOfUndefinedVariable dname
  | UnusedVariable (dname, _)
      -> Some (Ast.info_of_dname dname)

  | UseOfUndefinedMember name 
      -> Some (Ast.info_of_name name)

  | UglyGlobalDynamic info
  | WeirdForeachNoIteratorVar info
      -> Some info

  | CfgError err ->
      Controlflow_build_php.info_of_error err
  | CfgPilError err ->
      Controlflow_build_pil.info_of_error err

let report_error err = 
  pr2 (string_of_error err)

(*****************************************************************************)
(* Global bis *)
(*****************************************************************************)

let _errors = ref []

(* todo? let exn_when_error *)

let fatal err =
  Common.push2 err _errors
(* no difference for now ... *)
let warning err = 
  Common.push2 err _errors

let report_all_errors () = 
  !_errors |> List.rev |> List.iter report_error


(*****************************************************************************)
(* Ranking bis *)
(*****************************************************************************)

(* ranking errors, inspired by Engler slides *)
let rank_errors errs =
  errs +> List.map (fun x ->
    x,
    match x with
    | UnusedVariable (_, Scope_code.Local) -> 10
    | CfgError (Controlflow_build_php.DeadCode (_, node_kind)) ->
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
    match err with
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

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let (h_already_error: ((Entity_php.id_kind * string, bool) Hashtbl.t)) = 
  Hashtbl.create 101 

let (find_entity:
  find_entity: Ast_entity_php.entity_finder option ->
  (Entity_php.id_kind * Ast_php.name) ->
  Ast_entity_php.id_ast option) = 
 fun ~find_entity (kind, name) ->

  match find_entity with
  | None -> None
  | Some find_entity ->
      let str = Ast.name name in
      let ids_ast = find_entity (kind, str) in
      (match ids_ast with
      | [x] -> Some x
      | [] ->
          fatal (UndefinedEntity (kind, name));
          None
      | x::y::xs ->
          if Hashtbl.mem h_already_error (kind, str) 
          then ()
          else begin
            Hashtbl.replace h_already_error (kind, str) true;
            (* todo: to give 2 ex of defs *)
            let ex1 = name (* TODO *) in
            let ex2 = name (* TODO *) in
            fatal (MultiDefinedEntity (kind, name, (ex1, ex2)));
          end;
          (* can give the first one ... *)
          Some x
      )
