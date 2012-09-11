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

open Ast_php_simple
module Ast = Ast_php_simple

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
 *  Root -> Dir -> File (.php) -> # TODO more fine grained dependencies
 *       -> Dir -> SubDir -> Module? -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for the extract_uses visitor *)
type env = {
  current: Graph_code.node;
  g: Graph_code.graph;

  (* error reporting *)
  dupes: (Graph_code.node) Common.hashset;
  lookup_fails: (Graph_code.node, int) Common.hash_with_default;
  (* todo: dynamic_fails stats *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file = 
  try 
    let cst = Parse_php.parse_program file in
    let ast = Ast_php_simple_build.program cst in
    ast
  with exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    []

let add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (Ast.str_of_name name, kind) in
  (match () with
  (* maybe nested function, in which case we dont have the def *)
  | _ when not (G.has_node src env.g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist (nested func?)"
              (G.string_of_node src) (G.string_of_node dst));
      ()
  (* we skip reference to dupes *)
  | _ when Hashtbl.mem env.dupes src || Hashtbl.mem env.dupes dst -> ()
  (* todo: if n2 is a Class, then try Interface and Trait if fails? *)
  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g
  | _ -> 
      (* todo: debug, display edge? *)
      env.lookup_fails#update dst Common.add1
  )

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~dupes ~ast ~readable =
  let dir = Common.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g +> G.add_node (readable, E.File);
  g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  (* less? what about nested classes/functions/defines? *)
  ast +> List.iter (function

  | FuncDef def ->
      let node = (Ast.str_of_name def.f_name, E.Function) in
      if G.has_node node g then Hashtbl.add dupes node true
      else begin
        g +> G.add_node node;
        g +> G.add_edge ((readable, E.File), node) G.Has;
      end

  | ConstantDef def ->
      let node = (Ast.str_of_name def.cst_name, E.Constant) in
      if G.has_node node g then Hashtbl.replace dupes node true
      else begin
        g +> G.add_node node;
        g +> G.add_edge ((readable, E.File), node) G.Has;
      end
  (* old style constant definition, before PHP 5.4 *)
  | Expr(Call(Id("define", _), [String((s,_)); v])) ->
      let node = (s, E.Constant) in
      if G.has_node node g then Hashtbl.replace dupes node true
      else begin
        g +> G.add_node node;
        g +> G.add_edge ((readable, E.File), node) G.Has;
      end

  | ClassDef def ->
      let kind = 
        match def.c_kind with
        | ClassRegular | ClassFinal | ClassAbstract -> E.RegularClass
        | Interface -> E.Interface
        | Trait -> E.Trait
      in
      let node = (Ast.str_of_name def.c_name, E.Class kind) in
      if G.has_node node g then Hashtbl.replace dupes node true
      else begin
        g +> G.add_node node;
        g +> G.add_edge ((readable, E.File), node) G.Has;
        (* todo: visit methods/fields *)
      end;
       

  (* could add entity for that? *)
  | Global _ -> ()

  | StaticVars _ -> ()

  | Expr _ | Block _
  | If _ | Switch _
  | While _ | Do _ | For _ | Foreach _
  | Return _ | Break _ | Continue _
  | Throw _ | Try _
    -> ()
  );
  ()

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)
let rec extract_uses ~g ~ast ~dupes ~readable ~lookup_fails =
  let env = {
    current = (readable, E.File);
    g;
    dupes; lookup_fails;
  } 
  in
  stmtl env ast;

(* ---------------------------------------------------------------------- *)
(* Functions/Methods *)
(* ---------------------------------------------------------------------- *)
and func_def env def =
  def.f_params +> List.iter (fun p ->
    (* todo: add deps to type hint? *)
    Common.opt (expr env) p.p_default;
  );
  stmtl env def.f_body

and class_def env def =
  def.c_extends +> Common.do_option (fun c2 ->
    add_use_edge env (c2, E.Class E.RegularClass);
  );
  def.c_implements +> List.iter (fun c2 ->
    add_use_edge env (c2, E.Class E.Interface);
  );
  def.c_uses +> List.iter (fun c2 ->
    add_use_edge env (c2, E.Class E.Trait);
  );

  def.c_constants +> List.iter (fun def ->
    expr env def.cst_body;
  );
  def.c_variables +> List.iter (fun def ->
    Common.opt (expr env) def.cv_value
  );
  def.c_methods +> List.iter (fun def ->
    stmtl env def.f_body
  )

and constant_def env def =
  expr env def.cst_body

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  (* boilerplate *)
  | FuncDef def -> 
      let n = (Ast.str_of_name def.f_name, E.Function) in
      func_def { env with current = n} def
  | ClassDef def -> 
      let kind = 
        match def.c_kind with
        | ClassRegular | ClassFinal | ClassAbstract -> E.RegularClass
        | Interface -> E.Interface
        | Trait -> E.Trait
      in
      let n = (Ast.str_of_name def.c_name, E.Class kind) in
      class_def { env with current = n} def
  | ConstantDef def -> 
      let n = (Ast.str_of_name def.cst_name, E.Constant) in
      constant_def { env with current = n} def

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
  | Global xs -> exprl env xs

(* todo: deps to class name? *)
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
(* Expr *)
(* ---------------------------------------------------------------------- *)
and expr env = function
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
      if Ast.is_variable name 
      then ()
      else add_use_edge env (name, E.Constant)

  | Call (e, es) ->
      (match e with
      (* simple function call *)
      | Id name when not (Ast.is_variable name) ->
          add_use_edge env (name, E.Function)

      (* static method call *)
      | Class_get (Id name1, Id name2) 
          when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
          let _aclass = Ast.str_of_name name1 in
          let _amethod = Ast.str_of_name name2 in
          add_use_edge env (name1, E.Class E.RegularClass)

      (* object call *)
      | Obj_get (e1, Id name2) 
          when not (Ast.is_variable name2) ->
          expr env e1

      (* todo: increment dynamic_fails stats *)
      | _ -> expr env e
      );
      exprl env es

  (* This should be executed only for field access. Calls should have
   * been catched in the Call pattern above.
   *)
  | Class_get (e1, e2) ->
      (match e1, e2 with
      | Id name1, Id name2
        when not (Ast.is_variable name1) && not (Ast.is_variable name2) ->
          add_use_edge env (name1, E.Class E.RegularClass)
      | e1, Id name2 when not (Ast.is_variable name2) ->
          expr env e1;

      | _ -> 
          exprl env [e1; e2]
      );
  (* Same, should be executed only for field access *)
  | Obj_get (e1, e2) ->
      (match e1, e2 with
      | _, Id name2 when (Ast.is_variable name2) ->
          expr env e1;
      | _ ->
          exprl env [e1; e2]
      );

  | New (e, es) ->
      expr env (Call (Class_get(e, Id ("__construct", None)), es))

  (* boilerplate *)
  | List xs -> exprl env xs
  | Assign (_, e1, e2) -> exprl env [e1;e2]
  (* todo? *)
  | InstanceOf (e1, e2) -> exprl env [e1;e2]
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

and array_value env = function
  | Aval e -> expr env e
  | Akval (e1, e2) -> exprl env [e1; e2]  

and xml env x =
  x.xml_attrs +> List.iter (fun (name, xhp_attr) -> expr env xhp_attr);
  x.xml_body +> List.iter (xhp env)

and xhp env = function
  | XhpText s -> ()
  | XhpExpr e -> expr env e
  | XhpXml x -> xml env x

and exprl env xs = List.iter (expr env) xs
and array_valuel env xs = List.iter (array_value env) xs

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir =
  let root = Common.realpath dir in
  let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in

  (* step0?: filter noisy modules/files *)
  let files = all_files in

  let g = G.create () in
  g +> G.add_node G.root;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  let dupes = Hashtbl.create 101 in

  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.filename_without_leading_path root file in
      let ast = parse file in
      extract_defs ~g ~dupes ~ast ~readable;
      ()
   ));
  dupes +> Common.hashset_to_list +> List.iter (fun n ->
    pr2 (spf "DUPE: %s" (G.string_of_node n));
  );

  (* step2: creating the 'Use' edges, the uses *)
  let lookup_fails = Common.hash_with_default (fun () -> 0) in

  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     let ast = parse file in
     extract_uses ~g ~dupes ~ast ~readable ~lookup_fails;
   ));

  lookup_fails#to_list +> Common.sort_by_val_highfirst +> Common.take_safe 10 
  +> List.iter (fun (n, cnt) ->
    pr2 (spf "LOOKUP FAIL: %s (%d)" (G.string_of_node n) cnt)
  );
  
  g
