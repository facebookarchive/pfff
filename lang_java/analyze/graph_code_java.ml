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
 *  - merge overloaded methods? yes, alternative is to mangle the
 *    name of the method with the type (a la C++ linker)
 * 
 * schema:
 *   Package -> SubPackage -> Class -> Method
 *                                  -> Field
 *                                  -> Constant (static final)
 *                                  -> SubClass -> ...
 *                                  TODO enum
 *   (when have no package)
 *   Dir -> Subdir -> File 
 * 
 *   PB -> Not_Found -> Package -> SubPackage -> ...
 * 
 * todo: 
 *  - adjust graph to remove intermediate singleton? com.xxx?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* for the extract_defs_uses visitor *)
type env = {
  current: Graph_code.node;
  current_qualifier: Ast_java.qualified_ident;

  imported: (bool * qualified_ident) list;
  params_locals: string list;
  type_params_local: string list;

  phase: phase;
  g: Graph_code.graph;

  (* less: skip_edges *)
}

(* todo: put in graph_code.ml? *)
and phase = Defs | Uses

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

let str_of_name xs = 
  xs +> List.map (fun (_tyarg_todo, ident) -> Ast.unwrap ident) +> 
    Common.join "."

(* TODO *)
let long_ident_of_name xs = List.map snd xs

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

(*****************************************************************************)
(* Package lookup heuristic (deprecated) *)
(*****************************************************************************)

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

let (lookup_fully_qualified: 
  env -> Ast.qualified_ident -> Graph_code.node option) = 
 fun env xs ->
  let rec aux current xs =
    match xs with
    | [] -> Some current
    | x::xs ->
        let children = G.children current env.g in
        let str =
          match current with
          | ".", E.Dir -> (Ast.unwrap x)
          | s, _ -> s ^ "." ^ (Ast.unwrap x)
        in
        let new_current = 
          children +> Common.find_some_opt (fun (s2, kind) ->
            if str =$= s2
            then Some (s2, kind)
            else None
          ) in
        (match new_current with
        | None -> None
        | Some current -> aux current xs
        )
  in
  aux G.root xs

(* Look for entity (package/class/method/field) in list of imported
 * packages or in global scope. Return fully qualified entity.
 * 
 * Note that the code graph store nodes in fully qualified form.
 *)
let (lookup: env -> Ast.qualified_ident -> 
      Graph_code.node option) = fun env xs ->

  let full_xs = env.current_qualifier ++ xs in
  
  match xs with
  | [] -> raise Impossible
  | [x] ->
      let s = Ast.unwrap x in
      (match s with
      | "super" | "this" -> None
      | s ->
          lookup_fully_qualified env full_xs
      )
  | _ -> None


(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
(* Note that there is no ~dupe, Java code use packages and fully qualified
 * entity so there is very rarely name conflicts.
 *)
let rec extract_defs_uses ~phase ~g ~ast ~readable ~lookup_fails ~skip_edges =

  let env = {
    current =
      (match ast.package with
      | None -> (readable, E.File)
      | Some long_ident -> (str_of_qualified_ident long_ident, E.Package)
      );
    current_qualifier =
      (match ast.package with
      | None -> 
          (* old: let s = "__" ^ readable ^ "__" in s, Ast.fakeInfo s *)
          []
      | Some long_ident -> long_ident
      );
    params_locals = [];
    type_params_local = [];
    imported = (ast.imports ++
      (* we also automatically import the current package *)
      (match ast.package with
      | None -> []
      | Some long_ident -> [false, long_ident ++ ["*", Ast.fakeInfo "*"]]
      ));
    g;
    phase;
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
        (* old:
        let str = str_of_qualified_ident long_ident in
        g +> G.add_node (readable, E.File);
        g +> G.add_edge ((str, E.Package), (readable, E.File)) G.Has;
        *)
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
   * from the import to know where to look for first).
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
  let full_ident = env.current_qualifier ++ [def.cl_name] in
  let full_str = str_of_qualified_ident full_ident in
  if env.phase = Defs then begin
    (* less: def.c_type? *)
    env.g +> G.add_node (full_str, E.Class E.RegularClass);
    env.g +> G.add_edge (env.current, (full_str, E.Class E.RegularClass)) G.Has;
  end;
  let env = { env with
    current = (full_str, E.Class E.RegularClass);
    current_qualifier = full_ident;
    params_locals = [];
    (* TODO *)
    type_params_local = [];
  } 
  in
  (* todo: cl_extends, cl_implements Use *)
  decls env def.cl_body

(* Java allow some forms of overloading, so the same method name can be
 * used multiple times.
 *)
and method_decl env def =

  let full_ident = env.current_qualifier ++ [def.m_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  if env.phase = Defs then begin
    (* less: static? *)
    (* todo: for now we just collapse all methods with same name together *)
    if G.has_node (full_str, E.Method E.RegularMethod) env.g
    then ()
    else begin
      env.g +> G.add_node (full_str, E.Method E.RegularMethod);
      env.g +> G.add_edge (env.current, (full_str, E.Method E.RegularMethod)) G.Has;
    end
  end;
  let env = { env with
    current = (full_str, E.Method E.RegularMethod);
    (* no change to the qualifier, methods are not a namespace *)
    current_qualifier = env.current_qualifier;
    params_locals = def.m_formals +> List.map (fun v -> Ast.unwrap v.v_name);
    (* TODO *)
    type_params_local = [];
  } 
  in
  var env def.m_var;
  List.iter (var env) def.m_formals;
  (* todo: m_throws *)
  stmt env def.m_body


and field_decl env def =
  let full_ident = env.current_qualifier ++ [def.f_var.v_name] in
  let full_str = str_of_qualified_ident full_ident in
  let kind = 
    if Ast.is_final_static def.f_var.v_mods
    then E.Constant
    else E.Field
  in
  if env.phase = Defs then begin
    (* less: static? *)
    env.g +> G.add_node (full_str, kind);
    env.g +> G.add_edge (env.current, (full_str, kind)) G.Has;
  end;
  let env = { env with
    current = (full_str, kind);
    current_qualifier = env.current_qualifier
  } 
  in
  field env def

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
(* mostly boilerplate, control constructs don't introduce entities *)
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
          (* also add LocalClass case? no, 'lookup env ...' handles that *)
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
  | Name n ->
      if env.phase = Uses then begin
        let str = str_of_name n in
        (match () with
        | _ when List.mem str env.params_locals -> ()
        | _ -> 
            (match lookup env (long_ident_of_name n) with
            | Some n2 -> 
                pr2 ("FOUND: " ^ Common.dump n);
                add_use_edge env n2
            | None ->
                pr2 ("PB: " ^ Common.dump n);
                ()
            )
        )
      end

  | Literal _ -> ()

  | ClassLiteral t -> typ env t
  | NewClass (t, args, decls_opt) ->
      typ env t;
      exprs env args;
      (match decls_opt with
      | None -> ()
      | Some xs ->
          (* todo: let env = ??? gen anon class number? *)
          decls env xs
      )
  | NewQualifiedClass (e, id, args, decls_opt) ->
      pr2_gen (NewQualifiedClass (e, id, args, decls_opt));
      raise Todo
  | NewArray (t, args, i, ini_opt) ->
      typ env t;
      exprs env args;
      init_opt env ini_opt

  | Call (e, es) ->
      expr env e;
      exprs env es
  | Dot (e, id) ->
      (* todo: match e, and try lookup method/field *)
      expr env e;

  | ArrayAccess (e1, e2) -> exprs env [e1;e2]
  | Postfix (e, op) | Prefix (op, e) -> expr env e
  | Infix (e1, op, e2) -> exprs env [e1;e2]
  | Conditional (e1, e2, e3) -> exprs env [e1;e2;e3]
  | Assignment (e1, op, e2) -> exprs env [e1;e2]

  | Cast (t, e) -> 
      typ env t;
      expr env e
  | InstanceOf (e, tref) ->
      expr env e;
      typ env (TRef tref);
      
      
      


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
(* TODO, class names in it *)
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

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse ~show_parse_error:true file in
     extract_defs_uses ~phase:Defs ~g ~ast ~readable 
       ~lookup_fails ~skip_edges;
    ));

  (* step2: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse ~show_parse_error:false file in
     extract_defs_uses ~phase:Uses ~g ~ast ~readable
       ~lookup_fails ~skip_edges;
   ));
  g
