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
 * choices:
 *  - package-based or dir-based schema? Seems simpler to have
 *    to use packages.
 * 
 * 
 * schema:
 *   Package -> Package.SubPackage -> Class -> Subclass -> Method
 *                                                      -> Field
 *                                                      -> ...
 *   (when have no package)
 *   Dir -> Subdir -> File 
 * 
 *   PB -> Not_Found -> Package -> Package.SubPackage -> ...
 * 
 * todo: 
 *  - adjust graph to remove intermediate singleton? com.xxx?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  current_qualifier: Ast_java.qualified_ident;
  imported: (bool * qualified_ident) list;
  params_locals: string list;
  g: Graph_code.graph;
}

(* todo: put in graph_code.ml? *)
type phase = Defs | Uses

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

(* quite similar to create_intermediate_directories_if_not_present *)
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
          (match kind_original with 
          | E.Package ->
              let fake_package = 
                (Common.split "\\." name) +> List.map (fun s -> s^"2") in
              let dst = (Common.join "." fake_package, kind_original) in
              if not (G.has_node dst env.g)
              then begin 
                create_intermediate_packages_if_not_present 
                  env.g parent_target 
                  (fake_package +> List.map (fun s -> s,()));
                pr2 (spf "PB: lookup fail on %s (in %s)" 
                        (G.string_of_node dst) (G.string_of_node src));
              end;
              env.g +> G.add_edge (src, dst) G.Use;
              ()
          | _ ->
              pr2 (spf "PB: lookup fail on %s (in %s)" 
                      (G.string_of_node dst) (G.string_of_node src));
              G.add_node dst env.g;
              env.g +> G.add_edge (parent_target, dst) G.Has;
              env.g +> G.add_edge (src, dst) G.Use;
          )
      )
  )

let looks_like_package_name_part s =
  s =~ "[a-z]"

let looks_like_class_name s =
  s =~ "[A-Z]"

(* todo: use env to lookup for package, which will remove some
 * false positives and failures.
 *)
let rec package_of_long_ident_heuristics env (is_static, long_ident) =

  let starting_point = 
  match List.rev long_ident, is_static with
  (* import can have a trailing '*' *)
  | ("*",_)::xs,        false -> xs
  (* import static method means we need to skip the method and class *)
  | (s, _)::(s2,_)::xs, true 
      when looks_like_class_name s2 -> 
      xs
  (* sometimes people import nested classes, so have to skip two names *)
  | (s, _)::(s2, _)::xs, false 
      when looks_like_class_name s && looks_like_class_name s2 ->
      xs
  (* usual case *)
  | (s, _)::xs, false 
      when looks_like_class_name s ->
      xs
  (* some exceptions ... TODO put in skip_list instead? *)
  | ("drawable", _)::xs, false -> xs
  | _, _ -> 
      pr2_gen long_ident;
      raise Impossible
  in
  starting_point +> Common.drop_while (fun (s, _) -> looks_like_class_name s)
  +> List.rev

(*****************************************************************************)
(* Class/Package Lookup *)
(*****************************************************************************)

let lookup env long_ident =
  raise Todo

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)

let rec extract_defs_uses ~phase ~g ~ast ~dupes ~readable ~lookup_fails ~skip_edges =

  let env = {
    current =
      (match ast.package with
      | None -> (readable, E.File)
      | Some long_ident -> (str_of_qualified_ident long_ident, E.Package)
      );
    current_qualifier =
      (match ast.package with
      | None -> []
      | Some long_ident -> long_ident
      );
    params_locals = [];
    imported = (ast.imports ++
      (* we also automatically import the current package *)
      (match ast.package with
      | None -> []
      | Some long_ident -> [false, long_ident ++ ["*", Ast.fakeInfo "*"]]
      ));
    g;
  }
  in

  if phase = Defs then begin
    match ast.package with
    (* usually scripts, tests, or entry points *)
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
  end;

  if phase = Uses then begin
    ast.imports +> List.iter (fun (is_static, long_ident) ->
      (* TODO: need resolve and add in env that certain unqualified names
       * are ok.
       * add classname -> fully_qualified in env.
       * when .* ? need put all children of package.
       *)
      let package = 
        package_of_long_ident_heuristics env (is_static, long_ident) in
      let str = str_of_qualified_ident package in
      add_use_edge env (str, E.Package);
    );
  end;
  (* imports is not the only way to use external packages, one can
   * also just qualify the classname or static method so we need
   * to visit the AST and lookup classnames (possibly using information
   * from the import to know where to look for first.
   *)
  decls env ast.decls

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)
and decl env = function
  | Class def -> class_decl env def
  | Method def -> method_decl env def
  | Field def -> field_decl env def
  | Init (_is_static, st) -> stmt env st

and decls env xs = List.iter (decl env) xs

and class_decl env def =
  raise Todo

and method_decl env def =
  raise Todo

and field_decl env def =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
(* mostly boilerplate *)
and stmt env = function
  | Empty -> ()
  | Block xs -> stmts env xs
  | Expr e -> expr env e
  | If (e, st1, st2) ->
      expr env e;
      stmt env st1;
      stmt env st2;
  | Switch (e, xs) ->
      expr env e;
      xs +> List.iter (fun (cs, sts) -> 
        cases env cs;
        stmts env sts
      )
  | While (e, st) ->
      expr env e;
      stmt env st;
  | Do (st, e) ->
      expr env e;
      stmt env st;
  | For (x, st) ->
      for_control env x;
      stmt env st;
  (* could have an entity and dependency ... but it's intra procedural
   * so not that useful
   *)
  | Label (_id, st) -> stmt env st
  | Break _idopt | Continue _idopt -> ()
  | Return eopt -> exprs env (Common.option_to_list eopt)
  | Sync (e, st) ->
      expr env e;
      stmt env st;
  | Try (st, xs, stopt) ->
      stmt env st;
      catches env xs;
      stmts env (Common.option_to_list stopt);
  | Throw e -> expr env e
  | Assert (e, eopt) ->
      exprs env (e::Common.option_to_list eopt)
  | LocalVar f -> field env f
  | LocalClass def -> class_decl env def

and stmts env xs = 
  let rec aux env = function
    | [] -> ()
    | x::xs -> 
        stmt env x;
        let env = 
          match x with
          | LocalVar fld -> 
              let str = Ast.unwrap fld.f_var.v_name in
              { env with params_locals = str::env.params_locals }
          (* todo: LocalClass => also add? *)
          | _ -> env
        in
        aux env xs
  in 
  aux env xs

and cases env xs = List.iter (case env) xs
and case env = function
  | Case e -> expr env e
  | Default -> ()

and for_control env () = ()

and catches env xs = List.iter (catch env) xs
and catch env (v, st) =
  var env v;
  let str = Ast.unwrap v.v_name in
  let env = { env with params_locals = str::env.params_locals } in
  stmt env st

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
  (* main dependency source! *)
  | Name n -> ()
  | _ -> ()

and exprs env xs = List.iter (expr env) xs
and init env = function
  | ExprInit e -> expr env e
  | ArrayInit xs -> List.iter (init env) xs
and init_opt env opt =
  match opt with
  | None -> ()
  | Some ini -> init env ini

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
(* TODO *)
and typ env x = 
  ()

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)
and var env v =
  typ env v.v_type;
  ()

and field env f =
  var env f.f_var;
  init_opt env f.f_init;
  ()

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

  let lookup_fails = Common.hash_with_default (fun () -> 0) in
  let skip_edges = skip_list +> Common.map_filter (function
    | Skip_code.Edge (s1, s2) -> Some (s1, s2)
    | _ -> None
  ) +> Common.hash_of_list 
  in
  let dupes = Hashtbl.create 101 in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse ~show_parse_error:true file in
     extract_defs_uses ~phase:Defs ~g ~dupes ~ast ~readable 
       ~lookup_fails ~skip_edges;
    ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
    g +> G.remove_edge (G.parent n g, n) G.Has;
    g +> G.add_edge (G.dupe, n) G.Has;
  );

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Uses ~g ~dupes ~ast ~readable
       ~lookup_fails ~skip_edges;
   ));

  g
