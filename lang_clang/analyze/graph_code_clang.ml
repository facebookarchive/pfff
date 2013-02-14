(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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

open Ast_clang
open Parser_clang

module Ast = Ast_clang
module Loc = Location_clang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Clang ASTs. See graph_code.ml and
 * main_codegraph.ml for more information.
 * 
 * 
 * schema:
 *  Root -> Dir -> File (.c|.h) -> Type (struct | enum | union) TODO use Class?
 *                                 -> Field
 *                                 -> ClassConstant (enum)
 *                              -> Function | Prototype
 *                              -> Type (for Typedef)
 *                              -> Global
 *       -> Dir -> SubDir -> ...
 * 
 * todo: 
 *  - proper defs for anon struct and union
 *  - Use for types, fields, globals
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  phase: phase;

  cnt: int ref;

  current: Graph_code.node;

  dir: Common.dirname;
  readable_clang_file: Common.filename;
  (* we now prefer to use uninclude_clang.ml *)
  current_c_file_DEPRECATED: Common.filename ref;

  at_toplevel: bool;
  (* todo: static_func_rename: ... Hashtbl.t; *)

  (* for error reports *)
  current_clang_file: Common.filename;
  line: int;

  log: string -> unit;
  pr2_and_log: string -> unit;
}
 and phase = Defs | Uses

let unknown_location = "Unknown_Location", E.File

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let parse2 file = 
  (* clang2_old: Parse_clang.parse file *)
  Common.get_value file
let parse a = 
  Common.profile_code "Parse_clang.parse" (fun () -> parse2 a)

(*****************************************************************************)
(* Filename helpers *)
(*****************************************************************************)

let unchar s =
  if s =~ "'\\(.*\\)'"
  then Common.matched1 s
  else failwith ("unchar pb: " ^ s)

(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)

let add_node_and_edge_if_defs_mode env node =
  let (str, kind) = node in
  let str' =
    match kind, env.current with
    | E.Field, (s, E.Type) ->
        s ^ "." ^ str
    | _ -> str
  in
  let node = (str', kind) in

  if env.phase = Defs then begin
    if G.has_node node env.g
    then
      (match kind with
      | E.Function | E.Prototype -> 
          env.pr2_and_log (spf "DUPE entity: %s" (G.string_of_node node))
      (* todo: have no Use for now for the other entities so skip errors *) 
      | _ -> ()
      )
    else begin
      env.g +> G.add_node node;
      env.g +> G.add_edge (env.current, node) G.Has;
    end
  end;
  { env with current = node }

(*****************************************************************************)
(* Add edge *)
(*****************************************************************************)

let rec add_use_edge env (s, kind) =
  let src = env.current in
  let dst = (s, kind) in

  if G.has_node dst env.g
  then  G.add_edge (src, dst) G.Use env.g
  else 
    (match kind with
    (* look for Prototype if no Function *)
    | E.Function -> add_use_edge env (s, E.Prototype)
    | _ ->
        env.pr2_and_log (spf "Lookup failure on %s (in %s)"
                            (G.string_of_node dst)
                            (env.current_clang_file))
    )


(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses env ast =
  let env = { env with readable_clang_file = 
      Common.filename_without_leading_path env.dir env.current_clang_file;
  }
  in
  let readable = env.readable_clang_file in
  let readable =
    if readable =~ "\\(.*\\).clang2"
    then Common.matched1 readable
    else readable
  in
      
  if env.phase = Defs then begin
    let dir = Common.dirname readable in
    G.create_intermediate_directories_if_not_present env.g dir;
    let node = (readable, E.File) in
    env.g +> G.add_node node;
    env.g +> G.add_edge ((dir, E.Dir), node) G.Has;
  end;
  let env = { env with current = (readable, E.File) } in
  (match ast with
  | Paren (TranslationUnitDecl, l, _loc::xs) ->
      List.iter (sexp_toplevel env) xs
  | _ -> failwith (spf "%s: not a TranslationDecl" env.current_clang_file)
  )

and sexp_toplevel env x =
  match x with
  | Paren (enum, l, xs) ->
      let env = 
        { env with line = l } in
      (let file_opt = 
        Loc.location_of_paren_opt env.current_clang_file (enum, l, xs) in
      file_opt +> Common.do_option (fun f ->
          env.current_c_file_DEPRECATED := f
      ));

      (* dispatch *)
      (match enum with
      | FunctionDecl | VarDecl
      | TypedefDecl | RecordDecl | EnumDecl 
      | FieldDecl | EnumConstantDecl
        -> decl env (enum, l, xs)
      | CallExpr 
        -> expr env (enum, l, xs)
      | _ -> 
          sexps env xs
      )
  | Angle (xs) ->
      sexps env xs
  | Anchor (xs) ->
      sexps env xs
  | Bracket (xs) ->
      sexps env xs
  | T tok ->
      ()

and sexp env x =
  sexp_toplevel {env with at_toplevel = false} x

and sexps env xs = List.iter (sexp env) xs

(* ---------------------------------------------------------------------- *)
(* Decls *)
(* ---------------------------------------------------------------------- *)

and decl env (enum, l, xs) =
  let env =
    match enum, xs with
    (* todo: look for static? *)
    | FunctionDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::rest ->
        let kind = 
          if rest +> List.exists (function 
          | Paren (CompoundStmt, _, _) -> true
          | _ -> false
          )
          then E.Function
          else E.Prototype
        in
        add_node_and_edge_if_defs_mode env (s, kind)

    (* todo: what about extern *)
    | VarDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::_rest ->
        if env.at_toplevel 
        then add_node_and_edge_if_defs_mode env (s, E.Global)
        else env

    (* I am not sure about the namespaces, so I prepend strings *)
    | TypedefDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::_rest ->
        add_node_and_edge_if_defs_mode env ("t__" ^ s, E.Type)
    | EnumDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("e__" ^ s, E.Type)
          
    | RecordDecl, _loc::(T (TLowerIdent "struct"))
        ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("s__" ^ s, E.Type)
    | RecordDecl, _loc::(T (TLowerIdent "union"))
        ::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env ("u__" ^ s, E.Type)
          
    (* usually embedded struct *)
    | RecordDecl, _loc::(T (TLowerIdent "struct"))::_rest ->
        incr env.cnt;
        add_node_and_edge_if_defs_mode env 
          (spf "s__anon__%d" !(env.cnt), E.Type)
          
    (* todo: usually there is a typedef just behind *)
    | EnumDecl, _loc::_rest ->
        incr env.cnt;
        add_node_and_edge_if_defs_mode env 
          (spf "e__anon__%d" !(env.cnt), E.Type)
    | RecordDecl, _loc::(T (TLowerIdent "union"))::_rest ->
        incr env.cnt;
        add_node_and_edge_if_defs_mode env 
          (spf "u__anon__%d" !(env.cnt), E.Type)

    | FieldDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env (s, E.Field)

    | FieldDecl, _loc::_rest ->
        incr env.cnt;
        add_node_and_edge_if_defs_mode env 
          (spf "f__anon__%d" !(env.cnt), E.Field)
    | EnumConstantDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_rest ->
        add_node_and_edge_if_defs_mode env (s, E.ClassConstant)
        
    | _ ->
        failwith (spf "%s:%d:wrong Decl line" 
                     env.current_clang_file env.line)
  in
  sexps env xs

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env (enum, l, xs) =
  (match enum, xs with
  | CallExpr, _loc::_typ::
      (Paren (ImplicitCastExpr, _l2, 
             _loc2::_typ2::Angle _::
               (Paren (DeclRefExpr, _l3,
                      _loc3::_typ3::T (TUpperIdent "Function")::T (THexInt _)
                        ::T (TString s)::_typ4::[]))::[]))
      ::_args ->
      let s = unchar s in
      if env.phase = Uses
      then add_use_edge env (s, E.Function)

  (* todo: unexpected form of call? function pointer call? *)
  | CallExpr, _ ->
      ()
  | _ -> raise Impossible
  );
  sexps env xs

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  (* clang2_old: *)
  let all_files = Lib_parsing_clang.find_source2_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files skip_list root all_files in
  (* step0: reorder files *)
  let files = Skip_code.reorder_files_skip_errors_last skip_list root files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  let chan = open_out (Filename.concat (Sys.getcwd()) "pfff.log") in

  let env = {
    g;
    phase = Defs;
    current = unknown_location;
    current_c_file_DEPRECATED = ref (fst unknown_location);
    current_clang_file = "__filled_later__";
    readable_clang_file = "__filled_later__";
    line = -1;
    cnt = ref 0;
    dir = root;
    at_toplevel = true;

    log = (fun s ->
        output_string chan (s ^ "\n");
        flush chan;
    );
    pr2_and_log = (fun s ->
      if verbose then pr2 s;
      output_string chan (s ^ "\n");
      flush chan;
    );
  } in
  G.add_node unknown_location g;
  G.add_edge (G.not_found, unknown_location) G.Has g;
  
  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with phase = Defs; current_clang_file = file} ast
   ));

  (* step2: creating the 'Use' edges *)
  env.pr2_and_log "\nstep2: extract Uses";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with phase = Uses; current_clang_file = file} ast
    ));
  g
