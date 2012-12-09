(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module G = Graph_code
module E = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Generating prolog DB facts from a graph_code.
 * 
 * less: could move stuff in a prolog_code.ml file.
 * 
 * For more information look at h_program-lang/database_code.pl
 * and its many predicates.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* mimics database_code.pl top comment *)
type fact =
  | At of entity * Common.filename (* readable path *) * int (* line *)
  | Kind of entity * Database_code.entity_kind

  (* todo? could use a record with 
   *  namespace: string list; 
   *  enclosing: string option;
   *  name: string
   *)
  and entity = 
   string list (* package/module/namespace/class qualifier*) * string (* name *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
let string_of_entity (xs, x) =
  match xs with
  | [] -> spf "'%s'" x
  | xs -> spf "('%s', '%s')" (Common.join "." xs) x
  
(* Quite similar to database_code.string_of_id_kind, but with lowercase
 * because of prolog atom convention. See also database_code.pl comment
 * about kind/2.
 *)
let string_of_entity_kind = function
  | E.Function -> "function"
  | E.Constant -> "constant"
  | E.Class x ->
      (match x with
      | E.RegularClass -> "class"
      | E.Interface -> "interface"
      | E.Trait -> "trait"
      )
  (* the static/1 predicate will say if static method (or class var) *)
  | E.Method _ -> "method"

  | E.ClassConstant -> "constant"
  | E.Field -> "field"

  | E.TopStmts  -> "stmtlist"
  | E.Other _ -> "idmisc"
  | E.Exception -> "exception"
  | E.Constructor -> "constructor"
  | E.Global -> "global"
  | E.Type -> "type"
  | E.Module -> "module"

  | (E.MultiDirs|E.Dir|E.File
    |E.Macro|E.Package
     ) ->
      raise Impossible

let string_of_fact fact =
  let s = 
    match fact with
    | Kind (entity, kind) ->
        spf "kind(%s, %s)" (string_of_entity entity) 
          (string_of_entity_kind kind)
    | At (entity, file, line) ->
      raise Todo
  in
  s ^ "."

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let entity_of_str s =
  let xs = Common.split "\\." s in
  match List.rev xs with
  | [] -> raise Impossible
  | [x] -> ([], x)
  | x::xs -> (List.rev xs, x)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build root g =

  let res = ref [] in
  let add x = Common.push2 x res in
  (* defs *)
  g +> G.iter_nodes (fun n ->
    let (str, kind) = n in
    match kind with
    | E.Function | E.Global | E.Constant | E.Type
    | E.Module
        -> add (Kind (entity_of_str str, kind))
    (* todo? field and constructor have a X.Y.type.fld so should
     * we generate for the entity a ([X;Y;type], fld) or ([X;Y], "type.fld")
     *)
    | E.Field | E.Constructor
        -> add (Kind (entity_of_str str, kind))
    | E.Exception
        -> add (Kind (entity_of_str str, kind))
    | E.Dir -> ()
    | _ -> 
        pr2_gen n;
        raise Todo
  );
  !res
