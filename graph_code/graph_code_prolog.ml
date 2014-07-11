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
  | Type of entity * string (* could be more structured ... *)

  | Extends of string * string 
  | Implements of string * string
  | Mixins of string * string

  | Privacy of entity * Database_code.privacy

  | Call of entity * entity
  | UseData of entity * entity

  | Misc of string

  (* todo? could use a record with 
   *  namespace: string list; 
   *  enclosing: string option;
   *  name: string
   *)
  and entity = 
   string list (* package/module/namespace/class/struct/type qualifier*) * 
   string (* name *)

(*****************************************************************************)
(* IO *)
(*****************************************************************************)
(* todo: hmm need to escape x no? In OCaml toplevel values can have a quote
 * in their name, like foo'', which will not work well with Prolog atoms.
 *)

(* http://pleac.sourceforge.net/pleac_ocaml/strings.html *)
let escape charlist str =
  let rx = Str.regexp ("\\([" ^ charlist ^ "]\\)") in
  Str.global_replace rx "\\\\\\1" str

let escape_quote_and_double_quote s = escape "'\"" s

let string_of_entity (xs, x) =
  match xs with
  | [] -> spf "'%s'" (escape_quote_and_double_quote x)
  | xs -> spf "('%s', '%s')" (Common.join "." xs) 
    (escape_quote_and_double_quote x)
  
(* Quite similar to database_code.string_of_id_kind, but with lowercase
 * because of prolog atom convention. See also database_code.pl comment
 * about kind/2.
 *)
let string_of_entity_kind = function
  | E.Function -> "function"
  | E.Constant -> "constant"
  | E.Class -> "class"
  | E.Method -> "method"

  | E.ClassConstant -> "constant"
  | E.Field -> "field"

  | E.TopStmts  -> "stmtlist"
  | E.Other _ -> "idmisc"
  | E.Exception -> "exception"
  | E.Constructor -> "constructor"
  | E.Global -> "global"
  | E.Type -> "type"
  | E.Module -> "module"
  | E.Package -> "package"
  | E.Prototype -> "prototype"
  | E.GlobalExtern -> "global_extern"

  | (E.MultiDirs|E.Dir|E.File|E.Macro) ->
      raise Impossible

let string_of_fact fact =
  let s = 
    match fact with
    | Kind (entity, kind) ->
        spf "kind(%s, %s)" (string_of_entity entity) 
          (string_of_entity_kind kind)
    | At (entity, file, line) ->
        spf "at(%s, '%s', %d)" (string_of_entity entity) file line
    | Type (entity, str) ->
        spf "type(%s, '%s')" (string_of_entity entity) 
          (escape_quote_and_double_quote str)

    | Extends (s1, s2) ->
        spf "extends('%s', '%s')" s1 s2
    | Mixins (s1, s2) ->
        spf "mixins('%s', '%s')" s1 s2
    | Implements (s1, s2) ->
        spf "implements('%s', '%s')" s1 s2

    | Privacy (entity, p) ->
      let predicate = 
        match p with
        | E.Public -> "is_public"
        | E.Private -> "is_private"
        | E.Protected -> "is_protected"
      in
      spf "%s(%s)" predicate (string_of_entity entity)

    (* less: depending on kind of e1 we could have 'method' or 'constructor'*)
    | Call (e1, e2) ->
        spf "docall(%s, %s, function)" 
          (string_of_entity e1) (string_of_entity e2)
    | UseData (e1, e2) ->
        spf "use(%s, %s, field)" 
          (string_of_entity e1) (string_of_entity e2)

    | Misc s -> s
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

let build g =

  let res = ref [] in
  let add x = Common.push x res in

  add (Misc "% -*- prolog -*-");
  add (Misc ":- discontiguous kind/2, at/3");
  add (Misc ":- discontiguous extends/2, implements/2");
  add (Misc ":- discontiguous docall/3");
  add (Misc ":- discontiguous use/3");

  (* defs *)
  g +> G.iter_nodes (fun n ->
    let (str, kind) = n in
    (match kind with
    | E.Function | E.Global | E.Constant | E.Type
    | E.Package | E.Module
    (* todo? field and constructor have a X.Y.type.fld so should
     * we generate for the entity a ([X;Y;type], fld) or ([X;Y], "type.fld")
     *)
    | E.Field | E.Constructor
    | E.Method | E.ClassConstant
    | E.Exception
        -> add (Kind (entity_of_str str, kind))
    (* todo: interface | trait *)
    | E.Class -> 
        add (Kind (entity_of_str str, kind))

    | E.File | E.Dir
    | E.Prototype | E.GlobalExtern 
      -> ()

    | (E.Macro|E.TopStmts|E.Other _|E.MultiDirs) ->
        pr2_gen n;
        raise Todo
    );
    (try 
      let nodeinfo = G.nodeinfo n g in
      add (At (entity_of_str str, 
               nodeinfo.G.pos.Parse_info.file,
               nodeinfo.G.pos.Parse_info.line))
     with Not_found -> ()
    );
  );

  (* uses *)

  (* we iter on the Use edges of the graph_code (see graph_code.ml), which
   * contains the inheritance tree, call graph, and data graph information.
   *)
  g +> G.iter_use_edges (fun n1 n2 ->
    match n1, n2 with
    (* less: at some point have to differentiate Extends and Implements
     * depending on the _kind, but for now let's simplify and convert
     * everything in regular class inheritance
     *)
    | ((s1, E.Class), (s2, E.Class)) ->
      add (Extends (s1, s2))
    | ((s1, E.Method), (s2, E.Method)) ->
      add (Call (entity_of_str s1, entity_of_str s2))

    | ((s1, E.Function), (s2, (E.Function | E.Prototype))) ->
        add (Call (entity_of_str s1, entity_of_str s2))

    | ((s1, (E.Function | E.Method | E.Global | E.Constant)), 
       (s2, (E.Field | E.ClassConstant) )) ->
        add (UseData (entity_of_str s1, entity_of_str s2))

    | _ -> ()
  );

  List.rev !res

(* old: was for PHP.

  (* defs *)
  g +> G.iter_nodes (fun n ->
    let (str, kind) = n in
    (match kind with
    | E.Function | E.Constant | E.Class _ 
    | E.Global
    | E.ClassConstant
        -> add (P.Kind (P.entity_of_str str, kind))

    | E.Method _ ->
      add (P.Kind (P.entity_of_str str, kind));
      let nodeinfo = G.nodeinfo n g in
      let props = nodeinfo.G.props in
      props +> List.iter (function
      | E.Privacy priv ->
        add (P.Privacy (P.entity_of_str str, priv));
      | _ -> ()
      );

    | E.Field ->
      let (xs, x) = P.entity_of_str str in
      if x =~ "\\$\\(.*\\)"
      then add (P.Kind ((xs, Common.matched1 x), kind))
      else failwith ("field does not contain $: " ^ x)

    | E.File -> ()
    | E.Dir -> ()

    | _ ->
        pr2_gen n;
        raise Todo
    );
    (try 
      let nodeinfo = G.nodeinfo n g in
      add (P.At (P.entity_of_str str, 
               nodeinfo.G.pos.Parse_info.file,
               nodeinfo.G.pos.Parse_info.line))
    with Not_found -> ()
    );
  );

  (* uses *)

  (* we iter on the Use edges of the graph_code (see graph_code.ml), which
   * contains the inheritance tree, call graph, and data graph information.
   *)
  g +> G.iter_use_edges (fun n1 n2 ->
    match n1, n2 with
    | ((s1, E.Class _kind1), (s2, E.Class E.RegularClass)) ->
      add (P.Extends (s1, s2))
    | ((s1, E.Class _kind1), (s2, E.Class E.Trait)) ->
      add (P.Mixins (s1, s2))
    | ((s1, E.Class _kind1), (s2, E.Class E.Interface)) ->
      add (P.Implements (s1, s2))

    | ((s1, E.Function), (s2, E.Function)) ->
      add (P.Misc (spf "docall(%s, %s, function)" s1 s2))

    | _ -> ()
  );
*)
