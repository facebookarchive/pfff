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

module Ast = Ast_php_simple
open Ast_php_simple

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for PHP. See graph_code.ml and main_codegraph.ml
 * for more information. Yet another code database for PHP ...
 * 
 * See also old/graph_php.ml, facebook/flib_dependencies/,
 * facebook/check_module/graph_module.ml
 * 
 * schema:
 *  Root -> Dir -> File (.php) -> Class (interfaces and traits too)
 *                                 -> Method
 *                                 -> Field
 *                                 -> ClassConstant
 *                             -> Function
 *                             -> Constant
 *       -> Dir -> SubDir -> File -> ...
 *       -> Dir -> SubDir -> Module? -> ...
 * 
 * todo: 
 *  - add pos info in nodeinfo
 *  - handle Interface and Traits, do not translate them in RegularClass?
 *  - handle static vs non static methods/fields? but at the same time
 *    lots of our code abuse $this-> where they should use self::, so
 *    maybe simpler not make difference between static and non static
 *  - reuse env, most of of build() and put it in graph_code.ml
 *    and just pass the PHP specificities.
 *  - add tests
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;

  phase: phase;
  current: Graph_code.node;
  (* "NOSELF" when outside a class *)
  self: string;
  (* "NOPARENT" when no parent *)
  parent: string;

  (* we use the Hashtbl.find_all property *)
  skip_edges: (string, string) Hashtbl.t;

  (* right now used in extract_uses phase to transform a src like main()
   * into its File.
   *)
  dupes: (Graph_code.node) Common.hashset;
  (* todo: dynamic_fails stats *)
}
  (* We need 3 phases, one to get all the definitions, one to
   * get the inheritance information, and one to get all the Uses.
   * The inheritance is a kind of use, but certain uses like using
   * a field or method need the full inheritance tree to already be
   * computed as we may need to lookup entities up in the parents.
   *)
  and phase = Defs | Inheritance | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse2 file = 
  try 
    Common.save_excursion Ast_php_simple_build.store_position true (fun () ->
    let cst = Parse_php.parse_program file in
    let ast = Ast_php_simple_build.program cst in
    ast
    )
  with
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    []
let parse a = 
  (* on huge codebase naive memoization stresses too much the GC.
   * We marshall a la juju so the heap graph is smaller at least.
   *)
  Marshal.from_string
    (Common.memoized _hmemo a (fun () -> 
      Marshal.to_string (parse2 a) []
     )) 0


let add_node_and_edge_if_defs_mode env name_node =
  let (name, kind) = name_node in
  let str =
    match kind with
    | E.ClassConstant | E.Field | E.Method _ -> 
      env.self ^ "." ^ Ast.str_of_name name
    | _ -> Ast.str_of_name name
  in
  let node = (str, kind) in
  if env.phase = Defs then begin
    if G.has_node node env.g 
    then Hashtbl.replace env.dupes node true
    else begin
      env.g +> G.add_node node;
      (* if we later find a duplicate for node, we will
       * redirect this edge in build() to G.dupe.
       *)
      env.g +> G.add_edge (env.current, node) G.Has;
    end
  end;
  (* for nodes like main(), which will be dupes,
   * it's better to keep 'current' as the current File so
   * at least we will avoid some fail lookup
   *)
  if Hashtbl.mem env.dupes node
  then env
  else { env with current = node }



let rec add_use_edge env ((str, kind) as dst) =
  let src = env.current in
  (match () with
  (* maybe nested function, in which case we dont have the def *)
  | _ when not (G.has_node src env.g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist (nested func?)"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
      let (s1, _) = src in
      let (s2, _) = dst in
      if Hashtbl.mem env.skip_edges s1 &&
         List.mem s2 (Hashtbl.find_all env.skip_edges s1)
      then pr2 (spf "SKIPPING: %s --> %s" s1 s2)
      else 
        G.add_edge (src, dst) G.Use env.g

  | _ -> 
      (match kind with
      (* if dst is a Class, then try Interface *)
      (*
      | E.Class E.RegularClass -> 
          add_use_edge env (name, E.Class E.Interface)
      *)
      | _ ->
          let kind_original = kind in
          let dst = (str, kind_original) in

          G.add_node dst env.g;
          let parent_target = G.not_found in
          (match kind with
          (* TODO, fix those *)
          | E.Method _ | E.Field | E.ClassConstant -> 
            ()
          | _ ->
            pr2 (spf "PB: lookup fail on %s (in %s)" 
                   (G.string_of_node dst) (G.string_of_node src));
          );
          env.g +> G.add_edge (parent_target, dst) G.Has;
          env.g +> G.add_edge (src, dst) G.Use;
      )
  )

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

let lookup2 g (aclass, amethod_or_field_or_constant) =
  let rec depth current =
    if not (G.has_node current g)
    then None
    else 
      let children = G.children current g in
      let full_name = (fst current ^ "." ^ amethod_or_field_or_constant) in
      let res = 
        children +> Common.find_some_opt (fun (s2, kind) ->
          if full_name =$= s2
          then Some (s2, kind)
          else None
        )
      in
      match res with
      | Some x -> Some x
      | None ->
        let parents_inheritance = G.succ current G.Use g in
        breath parents_inheritance
  and breath xs = xs +> Common.find_some_opt depth
  in
  depth (aclass, E.Class E.RegularClass)

let lookup g a = 
  Common.profile_code "Graph_php.lookup" (fun () -> lookup2 g a)

(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses ~phase ~g ~ast ~dupes ~readable ~skip_edges =

  let env = {
    g; phase;
    current = (readable, E.File);
    self = "NOSELF"; parent = "NOPARENT";
    dupes;
    skip_edges;
  } 
  in

  if phase = Defs then begin
    let dir = Common.dirname readable in
    G.create_intermediate_directories_if_not_present g dir;

    g +> G.add_node (readable, E.File);
    g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;
  end;

  stmtl env ast;

(* ---------------------------------------------------------------------- *)
(* Stmt/toplevel *)
(* ---------------------------------------------------------------------- *)
and stmt env x =
  match x with
  (* boilerplate *)
  | FuncDef def -> func_def env def
  | ClassDef def -> class_def env def
  | ConstantDef def -> constant_def env def

  (* old style constant definition, before PHP 5.4 *)
  | Expr(Call(Id("define", _), [String((name)); v])) ->
     let node = (name, E.Constant) in
     let env = add_node_and_edge_if_defs_mode env node in
     expr env v

  | Expr e -> expr env e
  | Block xs -> stmtl env xs
  | If (e, st1, st2) ->
      expr env e;
      stmtl env [st1;st2]
  | Switch (e, xs) ->
      expr env e;
      casel env xs
  | While (e, xs) | Do (xs, e) ->
      expr env e;
      stmtl env xs
  | For (es1, es2, es3, xs) ->
      exprl env (es1 ++ es2 ++ es3);
      stmtl env xs
  | Foreach (e1, e2, e3opt, xs) ->
      exprl env [e1;e2];
      Common.opt (expr env) e3opt;
      stmtl env xs;
  | Return (_, eopt)  | Break eopt | Continue eopt ->
      Common.opt (expr env) eopt
  | Throw e -> expr env e
  | Try (xs, c1, cs) ->
      stmtl env xs;
      catches env (c1::cs)

  | StaticVars xs ->
      xs +> List.iter (fun (name, eopt) -> Common.opt (expr env) eopt;)
  (* could add entity for that? *)
  | Global xs -> exprl env xs

(* less: add deps to type hint? *)
and catch env (_hint_type, _name, xs) =
  stmtl env xs

and case env = function
  | Case (e, xs) ->
      expr env e;
      stmtl env xs
  | Default xs ->
      stmtl env xs

and stmtl env xs = List.iter (stmt env) xs
and casel env xs = List.iter (case env) xs
and catches env xs = List.iter (catch env) xs

(* ---------------------------------------------------------------------- *)
(* Defs *)
(* ---------------------------------------------------------------------- *)
and func_def env def =
  let node = (def.f_name, E.Function) in
  let env = 
    match def.f_kind with
    | AnonLambda -> env
    | Function -> add_node_and_edge_if_defs_mode env node 
    | Method -> raise Impossible
  in
  def.f_params +> List.iter (fun p ->
    (* less: add deps to type hint? *)
    Common.opt (expr env) p.p_default;
  );
  stmtl env def.f_body

and class_def env def =
  let kind = E.RegularClass in
  (*
    let _kind = 
    match def.c_kind with
    | ClassRegular | ClassFinal | ClassAbstract -> E.RegularClass
    | Interface -> E.Interface
    | Trait -> E.Trait
    in
  *)
  let node = (def.c_name, E.Class kind) in
  let env = add_node_and_edge_if_defs_mode env node in

  (* opti: could also just push those edges in a _todo ref during Defs *)
  if env.phase = Inheritance then begin
    def.c_extends +> Common.do_option (fun c2 ->
      add_use_edge env (Ast.str_of_name c2, E.Class E.RegularClass);
    );
    (* todo: use Interface and Traits at some point *)
    def.c_implements +> List.iter (fun c2 ->
      add_use_edge env (Ast.str_of_name c2, E.Class E.RegularClass);
    );
    def.c_uses +> List.iter (fun c2 ->
      add_use_edge env (Ast.str_of_name c2, E.Class E.RegularClass);
    );
  end;
  let self = Ast.str_of_name def.c_name in
  let parent = 
    match def.c_extends with 
    | None -> "NOPARENT" 
    | Some c2 -> Ast.str_of_name c2
  in
  let env = { env with self; parent } in
  
  def.c_constants +> List.iter (fun def ->
    let node = (def.cst_name, E.ClassConstant) in
    let env = add_node_and_edge_if_defs_mode env node in
    expr env def.cst_body;
  );
  def.c_variables +> List.iter (fun def ->
    let node = (def.cv_name, E.Field) in
    let env = add_node_and_edge_if_defs_mode env node in
    Common.opt (expr env) def.cv_value
  );
  def.c_methods +> List.iter (fun def ->
    (* less: be more precise at some point *)
    let kind = E.RegularMethod in
    let node = (def.f_name, E.Method kind) in
    let env = add_node_and_edge_if_defs_mode env node in
    stmtl env def.f_body
  )

and constant_def env def =
  let node = (def.cst_name, E.Constant) in
  let env = add_node_and_edge_if_defs_mode env node in
  expr env def.cst_body

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env x = 
  if env.phase = Uses then
  (match x with
  | Int _ | Double _  -> ()

  (* todo: this can hide actually name of functions or classes ... *)
  | String _ -> ()

  (* Note that you should go here only when it's a constant. You should
   * catch the use of Id in other contexts before. For instance you
   * should match on Id in Call, Class_get, etc so that this code
   * is executed really as a last resort, which usually means when
   * there is the use of a constant.
   *)
  | Id name -> 
      (* a parameter or local variable *)
      if Ast.is_variable name 
      then ()
      else add_use_edge env (Ast.str_of_name name, E.Constant)

  (* -------------------------------------------------- *)
  | Call (e, es) ->
    (match e with
    (* simple function call *)
    | Id name when not (Ast.is_variable name) ->
        add_use_edge env (Ast.str_of_name name, E.Function);
        exprl env es

    (* static method call *)
    | Class_get (Id ("__special__self", tok), e2) ->
        expr env (Call (Class_get (Id (env.self, tok), e2), es))
    | Class_get (Id ("__special__parent", tok), e2) ->
        expr env (Call (Class_get (Id (env.parent, tok), e2), es))

    | Class_get (Id name1, Id name2) 
        when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
         let aclass = Ast.str_of_name name1 in
         let amethod = Ast.str_of_name name2 in
         let node = (aclass ^ "." ^ amethod, E.Method E.RegularMethod) in
         (match lookup env.g (aclass, amethod) with
         | None -> 
           (match amethod with
           | "__construct" -> ()
           | _ -> add_use_edge env node
           )
         | Some n -> add_use_edge env n
         );
         exprl env es

    (* object call *)
    | Obj_get (e1, Id name2)  when not (Ast.is_variable name2) ->
        (match e1 with
        (* handle easy case *)
        | This (_,tok) ->
          expr env (Call (Class_get (Id (env.self, tok), Id name2), es))
        (* need class analysis ... *)
        | _ -> 
          (* todo: increment dynamic_fails stats *)
          expr env e1;
          exprl env es
        )
    (* todo: increment dynamic_fails stats *)
    | _ -> 
      expr env e; 
      exprl env es
    (* todo: increment dynamic_fails stats also when use func_call_args() *)
    )

  (* -------------------------------------------------- *)
  (* This should be executed only for access to class constants or static
   * class variable; calls should have been catched in the Call pattern above.
   *)
  | Class_get (e1, e2) ->
      (match e1, e2 with
      | Id ("__special__self", tok), _ ->
        expr env (Class_get (Id (env.self, tok), e2))
      | Id ("__special__parent", tok), _ ->
        expr env (Class_get (Id (env.parent, tok), e2))

      | Id name1, Id name2
        when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
          let aclass = Ast.str_of_name name1 in
          let aconstant = Ast.str_of_name name2 in
          let node = (aclass ^ "." ^ aconstant, E.ClassConstant) in
          (match lookup env.g (aclass, aconstant) with
          | None -> add_use_edge env node
          (* less: assert kind = ClassConstant? *)
          | Some n -> add_use_edge env n
          )

      | Id name1, Id name2
        when not (Ast.is_variable name1) && (Ast.is_variable name2) ->
          let aclass = Ast.str_of_name name1 in
          let astatic_var = Ast.str_of_name name2 in
          let node = (aclass ^ "." ^ astatic_var, E.Field) in
          (match lookup env.g (aclass, astatic_var) with
          | None -> add_use_edge env node
          (* less: assert kind = Static variable *)
          | Some n -> add_use_edge env n
          )
 
     (* todo: update dynamic stats *)
     | Id name1, e2 when not (Ast.is_variable name1) ->
          add_use_edge env (Ast.str_of_name name1, E.Class E.RegularClass);
          expr env e2;
     | e1, Id name2 when not (Ast.is_variable name2) ->
       expr env e1;
     | _ -> 
       exprl env [e1; e2]
      )

  (* same, should be executed only for field access *)
  | Obj_get (e1, e2) ->
      (match e1, e2 with
      (* handle easy case *)
      | This (_, tok), Id name2 when not (Ast.is_variable name2) ->
          let (s2, tok2) = name2 in
          expr env (Class_get (Id (env.self, tok), Id ("$" ^ s2, tok2)))
      | _, Id name2 when not (Ast.is_variable name2) ->
          expr env e1;
      | _ ->
          exprl env [e1; e2]
      )

  | New (e, es) ->
      expr env (Call (Class_get(e, Id ("__construct", None)), es))

  (* -------------------------------------------------- *)
  (* boilerplate *)
  | List xs -> exprl env xs
  | Assign (_, e1, e2) -> exprl env [e1;e2]
  | InstanceOf (e1, e2) -> 
      expr env e1;
      (match e2 with
      (* less: add deps? *)
      | Id name when not (Ast.is_variable name) -> 
          ()
      | _ -> 
          (* todo: update dynamic *)
          expr env e2
      )
          
  | This _ -> ()
  | Array_get (_, e, eopt) ->
      expr env e;
      Common.opt (expr env) eopt
  | Infix (_, e) | Postfix (_, e) | Unop (_, e) -> expr env e
  | Binop (_, e1, e2) -> exprl env [e1; e2]
  | Guil xs -> exprl env xs
  | Ref e -> expr env e
  | ConsArray (_, _, xs) -> array_valuel env xs
  | Xhp x -> xml env x
  | CondExpr (e1, e2, e3) -> exprl env [e1; e2; e3]
  (* less: again, add deps for type? *)
  | Cast (_, e) -> expr env e
  | Lambda def -> func_def env def
  )

and array_value env = function
  | Aval e -> expr env e
  | Akval (e1, e2) -> exprl env [e1; e2]  

(* todo: dependency to :x:y class? *)
and xml env x =
(* todo: dependency on field? *)
  x.xml_attrs +> List.iter (fun (name, xhp_attr) -> expr env xhp_attr);
  x.xml_body +> List.iter (xhp env)

and xhp env = function
  | XhpText s -> ()
  | XhpExpr e -> expr env e
  | XhpXml x -> xml env x

and exprl env xs = List.iter (expr env) xs
and array_valuel env xs = List.iter (array_value env) xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir skip_list =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in
  
  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in
  
  let g = G.create () in
  G.create_initial_hierarchy g;

  let dupes = Hashtbl.create 101 in
  let skip_edges = Skip_code.build_filter_edges skip_list in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse file in
      try 
        extract_defs_uses ~phase:Defs ~g ~dupes ~ast ~readable ~skip_edges;
      with Graph_code.Error Graph_code.NodeAlreadyPresent ->
        failwith (spf "Node already present in %s" file)
   ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
    g +> G.remove_edge (G.parent n g, n) G.Has;
    g +> G.add_edge (G.dupe, n) G.Has;
  );

  (* step2: creating the 'Use' edges for inheritance *)
  if verbose then pr2 "\nstep2: extract inheritance";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse file in
     extract_defs_uses ~phase:Inheritance ~g ~dupes ~ast ~readable ~skip_edges;
   ));

  (* step3: creating the 'Use' edges, the uses *)
  if verbose then pr2 "\nstep3: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse file in
     extract_defs_uses ~phase:Uses ~g ~dupes ~ast ~readable ~skip_edges;
   ));
  
  g
