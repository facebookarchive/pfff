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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Clang ASTs. See graph_code.ml and
 * main_codegraph.ml for more information.
 * 
 * 
 * schema:
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)

type env = {
  g: Graph_code.graph;
  phase: phase;

  cnt: int ref;

  current: Graph_code.node;
  current_c_file: Common.filename ref;

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

(* todo? memoize? but the files are huge, maybe could try to memoize
 * and share the many parts that are in common in all those .clang files
 * because they include the same files.
 *)
let parse2 file = 
  Parse_clang.parse file
let parse a = 
  Common.profile_code "Parse_clang.parse" (fun () -> parse2 a)

(*****************************************************************************)
(* Filename helpers *)
(*****************************************************************************)
type location =
  | File of Common.filename * int * int
  | Line of int * int
  | Col of int
  | Other

let location_of_angle env xs =
  match xs with
  | [Angle [T (TLowerIdent "invalid"); T (TLowerIdent "sloc")]] ->
      [Other]
  | xs ->
      let xxs = Common.split_gen_when 
        (function (T TComma)::xs -> Some xs | _ -> None) xs in
      xxs +> List.map (function
        | [T (TLowerIdent "line"); T TColon; T (TInt i1);T TColon; T(TInt i2)]->
            Line (s_to_i i1, s_to_i i2)
        | [T (TLowerIdent "col"); T TColon; T (TInt i);] ->
            Col (s_to_i i)
        | [T (TPath f); T TColon; T (TInt i1);T TColon; T (TInt i2)] ->
            File (f, s_to_i i1, s_to_i i2)
        | [Angle _; T TColon; T (TInt _);T TColon; T (TInt _)] ->
            Other
        | xs -> 
          pr2_gen xs;
          failwith (spf "wrong location format at line %d in %s" 
                      env.line env.current_clang_file)
      )

let readable_of_filename f =
  let xs = Common.split "/" f in
  let xs = 
    match xs with
    | "usr"::"include"::rest -> 
        "EXTERNAL"::"CORE"::rest

    | "Users"::"yoann.padioleau"::"local"::"clang_ast"::"clang-llvm"
      ::"llvm"::"Debug+Asserts"::"bin"::".."
      ::"lib"::"clang"::"3.3"::"include"::rest ->
        "EXTERNAL"::"CLANG"::rest
    | "System"::"Library"::"Frameworks"::rest -> 
        "EXTERNAL"::"MACOS"::rest

    (* todo: use env.dir? *)
    | "home"::"pad"::"local"::"lang-c"::"Chipmunk-Physics"::rest -> 
        rest
    | "home"::"pad"::"pfff"::"tests"::"clang"::"c"::rest ->
        rest
    | _ -> failwith ("unhandled prefix: " ^ f)
  in
  Common.join "/" xs

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
    then begin
      ()
    end
    else begin
      env.g +> G.add_node node;
      let current =
        if env.current =*= unknown_location
        then !(env.current_c_file), E.File
        else env.current
      in
      env.g +> G.add_edge (current, node) G.Has;
    end
  end;
  { env with current = node }

(*****************************************************************************)
(* Add edge *)
(*****************************************************************************)

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses env ast =
  sexp env ast

and sexp env x =
  match x with
  | Paren (enum, l, xs) ->

      let env = { env with line = l } in
      let location =
        (match enum, xs with
        | (Misc__Null__ | Misc__Capture__ | Misc__Cleanup__Block
          ), _ -> [Other]
        | _, Angle xs::_rest ->
            location_of_angle env xs
        | _ -> 
            failwith (spf "%s:%d: no location" env.current_clang_file env.line)
        )
      in
      let file_opt = 
        location +> Common.find_some_opt (function 
        | File (f, _,_) ->
            let readable = readable_of_filename f in
            Some readable
        | _ -> None
        )
      in
      if env.phase = Defs then begin
        file_opt +> Common.do_option (fun readable ->
          let dir = Common.dirname readable in
          G.create_intermediate_directories_if_not_present env.g dir;
          let node = (readable, E.File) in
          if not (G.has_node node env.g) then begin
            env.g +> G.add_node node;
            env.g +> G.add_edge ((dir, E.Dir), node) G.Has;
          end
        )
      end;
      (match file_opt with
      | None -> ()
      | Some f -> 
          env.current_c_file := f
      );
      (* dispatch *)
      (match enum with
      | FunctionDecl 
      | TypedefDecl | RecordDecl | EnumDecl 
      | FieldDecl | EnumConstantDecl
      (* | VarDecl | BlockDecl | ParmVarDecl    |   TranslationUnitDecl  *)
          ->
          decl env (enum, l, xs)
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

and sexps env xs = List.iter (sexp env) xs

(* ---------------------------------------------------------------------- *)
(* Decls *)
(* ---------------------------------------------------------------------- *)

and decl env (enum, l, xs) =
  let env =
    match enum, xs with
    | FunctionDecl, _loc::(T (TLowerIdent s | TUpperIdent s))::_typ_char::_rest ->
        add_node_and_edge_if_defs_mode env (s, E.Function)

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
  let all_files = Lib_parsing_clang.find_source_files_of_dir_or_files [root] in

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
    current_c_file = ref (fst unknown_location);
    current_clang_file = "__filled_later__";
    line = -1;
    cnt = ref 0;

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
      (* will modify env.dupes instead of raise Graph_code.NodeAlreadyPresent *)
      extract_defs_uses { env with phase = Defs; current_clang_file = file} ast
   ));

(*
  (* step2: creating the 'Use' edges for inheritance *)
  env.pr2_and_log "\nstep2: extract inheritance";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with phase = Uses; current_clang_file = file} ast
    ));
*)
  g
