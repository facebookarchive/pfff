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

module E = Database_code
module G = Graph_code

open Ast_c_simple
module Ast = Ast_c_simple

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for C. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * todo: there is different namespace in C: 
 *  - functions/locals,
 *  - tags (struct name, enum name)
 *  - ???
 * see ast_c.ml notes in coccinelle
 * 
 *  
 * less: reuse code with the other graph_code_xxx ?
 * 
 * schema:
 *  Root -> Dir -> File (.c|.h) -> Struct 
 *                                 #TODO fields, etc
 *                              -> Function
 *                              -> Constant
 *                              -> Macro
 *                              -> Type (for Typedef)
 *       -> Dir -> SubDir -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  g: Graph_code.graph;

  (* we use the Hashtbl.find_all property *)
  skip_edges: (string, string) Hashtbl.t;

  (* error reporting *)
  dupes: (Graph_code.node) Common.hashset;
  lookup_fails: (Graph_code.node, int) Common.hash_with_default;
  (* todo: dynamic_fails stats *)
}
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could maybe call Parse_c.parse to get the parsing
 * statistics
 *)

let parse file =
  try 
    (* less: make this parameters of parse_program? *) 
    Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.show_parsing_error false (fun () ->
    Common.save_excursion Flag.verbose_parsing false (fun () ->
    let cst = Parse_c.parse_program file in
    let ast = Ast_c_simple_build.program cst in
    ast
    )))
  with 
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    raise exn

let todo() =
  ()

let nodes_of_toplevel x =
  match x with
  | Define (name, _val) ->
      [(Ast.str_of_name name, E.Constant)]
  | Macro (name, _args, _body) -> 
      [(Ast.str_of_name name, E.Macro)]
  | FuncDef def -> 
      [(Ast.str_of_name def.f_name, E.Function)]
  | StructDef def -> 
      [(Ast.str_of_name def.s_name, E.Class E.RegularClass)]
  | TypeDef (name, _t) ->
      [(Ast.str_of_name name, E.Type)]
  | EnumDef def ->

      let (name, xs) = def in
      (* todo? add a __enum prefix? *)
      [(Ast.str_of_name name, E.Type)] ++
      (xs +> List.map (fun (name, eopt) ->
        (Ast.str_of_name name, E.Constant)
      ))

  (* todo: but need storage to see if extern or not? *)
  | Globals _ -> []
   (* todo: maybe letter, but need to find the real File
    * corresponding to the string, so may need some -I
    *)
  | Include _ -> []
  (* do we want them? *)
  | Prototype _ -> []

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~dupes ~ast ~readable =
  let dir = Common.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g +> G.add_node (readable, E.File);
  g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  ast +> List.iter (fun e ->
    let nodes = nodes_of_toplevel e in
    nodes +> List.iter (fun node ->
      if G.has_node node g 
      then Hashtbl.replace dupes node true
      else begin
        g +> G.add_node node;
        g +> G.add_edge ((readable, E.File), node) G.Has;
      end
    )
  )

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let rec extract_uses ~g ~ast ~dupes ~readable ~lookup_fails ~skip_edges =
  let env = {
    current = (readable, E.File);
    g;
    dupes; lookup_fails;
    skip_edges;
  }
  in
  toplevels env ast

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env x =
  match x with
  | Define (name, v) ->
      let n = (Ast.str_of_name name, E.Constant) in
      let env = { env with current = n } in
      (match v with
      | CppExpr e -> expr env e
      | CppStmt st -> stmt env st
      )

  | Macro _ -> todo()
  | FuncDef def -> todo()
  | StructDef def -> todo()
  | EnumDef def -> todo()

  (* todo: should analyze if s has the form "..." and not <> and
   * build appropriate link?
   *)
  | Include _ -> ()

  | TypeDef _ -> todo()
  | Globals _ -> todo()
 
  (* do we want them? *)
  | Prototype def -> ()

and toplevels env xs = List.iter (toplevel env) xs

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | Expr e -> expr env e

  | Asm e -> exprs env e

  | (Vars _|Goto _|Label (_, _)|Return _|For (_, _, _, _)|DoWhile (_, _)|
While (_, _)|Switch (_, _)|If (_, _, _)|Block _|Break|Continue) ->
      todo()
(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  | Int _ | Float _ | Char _ -> ()
  | String _  -> ()
 
  (* Note that you should go here only when it's a constant. You should
   * catch the use of Id in other contexts before. For instance you
   * should match on Id in Call, etc so that this code
   * is executed really as a last resort, which usually means when
   * there is the use of a constant.
   *)
  | Id name ->
      todo()

  | Call (e, es) -> todo()

  | InitList xs -> exprs env xs

  | SizeOf x ->
      (match x with
      | Left e -> expr env e
      | Right t -> type_ env t
      )
  | GccConstructor (t, e) ->
      type_ env t;
      expr env e

  | (Sequence (_, _)|CondExpr (_, _, _)
  | Binary (_, _, _)|Unary (_, _)|Infix (_, _)|Postfix (_, _)
  | Cast (_, _)|RecordAccess (_, _)
  | ArrayAccess (_, _)|Assign (_, _, _)
    ) -> todo()

and exprs env xs = List.iter (expr env) xs

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
and type_ env x =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_c.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  g +> G.add_node G.root;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  let dupes = Hashtbl.create 101 in
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse file in
      extract_defs ~g ~dupes ~ast ~readable;
    ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
  );

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  let lookup_fails = Common.hash_with_default (fun () -> 0) in
  let skip_edges = skip_list +> Common.map_filter (function
    | Skip_code.Edge (s1, s2) -> Some (s1, s2)
    | _ -> None
  ) +> Common.hash_of_list 
  in
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse file in
     extract_uses ~g ~dupes ~ast ~readable ~lookup_fails ~skip_edges;
   ));

  lookup_fails#to_list +> Common.sort_by_val_highfirst +> Common.take_safe 20
  +> List.iter (fun (n, cnt) ->
    pr2 (spf "LOOKUP FAIL: %s (%d)" (G.string_of_node n) cnt)
  );
  g
