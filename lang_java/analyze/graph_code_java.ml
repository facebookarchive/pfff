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

open Ast_java
module Ast = Ast_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for Java. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * todo:
 *  - package-based schema?
 *  - dir-based schema
 * 
 * schema:
 *)


(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  g: Graph_code.graph;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse ~show_parse_error file =
  try 
    Parse_java.parse_program file
  with 
  | Timeout -> raise Timeout
  | exn ->
      if show_parse_error
      then pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                        (Common.exn_to_s exn));

      { package = None; imports = []; decls = [] }

let str_of_qualified_ident xs =
  xs +> List.map Ast.unwrap +> Common.join "."

let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common.inits xs +> List.map str_of_qualified_ident in
  let dirs = 
    match dirs with
    | ""::xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x::xs ->
        let entity = x, E.Package in
        if G.has_node entity g
        then aux entity xs
        else begin
          g +> G.add_node entity;
          g +> G.add_edge (current, entity) G.Has;
          aux entity xs
        end
  in
  aux root dirs

let rec add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (name, kind) in
  (match () with
  | _ when not (G.has_node src env.g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g

  | _ -> 
      (match kind with
      | _ ->
          let kind_original = kind in
          let dst = (name, kind_original) in
          let parent_target = G.not_found in
          pr2 (spf "PB: lookup fail on %s (in %s)" 
                       (G.string_of_node dst) (G.string_of_node src));

          (match kind_original with 
          | E.Package ->
              let xs = 
                (Common.split "\\." name) +> List.map (fun x -> x, ()) 
              in
              create_intermediate_packages_if_not_present env.g parent_target 
                xs;
              pr2_gen xs;
              env.g +> G.add_edge (src, dst) G.Use;
              ()
          | _ ->
              G.add_node dst env.g;
              env.g +> G.add_edge (parent_target, dst) G.Has;
              env.g +> G.add_edge (src, dst) G.Use;
          )
      )
  )


(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

(* todo: adjust graph to remove intermediate singleton? com.xxx? *)
let extract_defs ~g ~dupes ~ast ~readable =
  (* package-based schema or dir-based? *)

  (match ast.package with
  | None ->
      let dir = Common.dirname readable in
      G.create_intermediate_directories_if_not_present g dir;
      g +> G.add_node (readable, E.File);
      g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  | Some long_ident ->
      create_intermediate_packages_if_not_present g G.root long_ident;
      let str = str_of_qualified_ident long_ident in
      g +> G.add_node (readable, E.File);
      g +> G.add_edge ((str, E.Package), (readable, E.File)) G.Has;
  )


(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let rec extract_uses ~g ~ast ~dupes ~readable ~lookup_fails ~skip_edges =
  
  let env = {
    current = (readable, E.File);
    g;
  }
  in

  ast.imports +> List.iter (fun (_is_static, long_ident) ->
    let package = 
      (match List.rev long_ident with
      | ("*",_)::xs -> xs
      | (s, _)::xs ->
          if s =~ "[A-Z].*" 
          then xs
          else begin
            pr2 (spf "PB: weird import: %s" (Common.dump long_ident));
            xs
          end
      | [] -> raise Impossible
      )
    in
    let str = str_of_qualified_ident (List.rev package) in
    add_use_edge env (str, E.Package);
  );

  (* imports is not the only way to use external packages, one can
   * also just qualify the classname or static method so we need
   * to visit the AST.
   *)
  ()

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
  let all_files = Lib_parsing_java.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  let dupes = Hashtbl.create 101 in
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse ~show_parse_error:true file in
      extract_defs ~g ~dupes ~ast ~readable;
    ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
    g +> G.remove_edge (G.parent n g, n) G.Has;
    g +> G.add_edge (G.dupe, n) G.Has;
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
     let ast = parse ~show_parse_error:false file in
     extract_uses ~g ~dupes ~ast ~readable ~lookup_fails ~skip_edges;
   ));

  g
