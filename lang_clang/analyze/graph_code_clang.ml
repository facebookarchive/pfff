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

  current: Graph_code.node;
  current_file: Common.filename;
  line: int;

  log: string -> unit;
  pr2_and_log: string -> unit;
}
 and phase = Defs | Uses


(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* todo? memoize? but the files are huge, maybe could try to memoize
 * and share the many parts that are in common in all those .clang files
 * because they include the same files.
 *)
let parse file = 
  Parse_clang.parse file

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
                      env.line env.current_file)
      )
      
(*****************************************************************************)
(* Add Node *)
(*****************************************************************************)

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
            failwith (spf "%s:%d: no location" env.current_file env.line)
        )
      in
      let _file_opt = 
        location +> Common.find_some_opt (function 
        | File (f, _,_) -> 
            pr2_once f;
            Some f
        | _ -> None
        )
      in
      sexps env xs
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
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

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
    current = ("__filled_later__", E.File);
    current_file = "__filled_later__";
    line = -1;

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
  
  (* step1: creating the nodes and 'Has' edges, the defs *)
  env.pr2_and_log "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      (* will modify env.dupes instead of raise Graph_code.NodeAlreadyPresent *)
      extract_defs_uses { env with phase = Defs; current_file = file} ast
   ));

(*
  (* step2: creating the 'Use' edges for inheritance *)
  env.pr2_and_log "\nstep2: extract inheritance";
  files +> Common_extra.progress ~show:verbose (fun k ->
    List.iter (fun file ->
      k();
      let ast = parse file in
      extract_defs_uses { env with phase = Uses; current_file = file} ast
    ));
*)
  g
