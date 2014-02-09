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

open Ast_c
module Ast = Ast_c

module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for C. See graph_code.ml and main_codegraph.ml
 * for more information.
 * See also lang_clang/analyze/graph_code_clang.ml to get a more
 * precise and correct graphcode (if you can afford yourself to 
 * use clang).
 * 
 * todo: there is different namespace in C: 
 *  - functions/locals,
 *  - tags (struct name, enum name)
 *  - ???
 *  - internals: remove lookup_fails, like in graph_code_php.ml
 * todo: fields? enum constants? 
 * todo: see ast_c.ml notes in coccinelle?
 * less: reuse code with the other graph_code_xxx ?
 * 
 * What about nested structures? they are lifted up in ast_c_build.
 *  
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
  g: Graph_code.graph;

  current: Graph_code.node;
  mutable params_locals: string list;

  (* error reporting *)
  dupes: (Graph_code.node) Common.hashset;
  (* less: less useful now with G.not_found *)
  lookup_fails: (Graph_code.node, int) Common2.hash_with_default;
  (* todo: dynamic_fails stats *)
}
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could maybe call Parse_c.parse to get the parsing
 * statistics
 *)

let parse ~show_parse_error file =
  try 
    (* less: make this parameters of parse_program? *) 
    Common.save_excursion Flag.error_recovery true (fun () ->
    Common.save_excursion Flag.show_parsing_error show_parse_error (fun () ->
    Common.save_excursion Flag.verbose_parsing show_parse_error (fun () ->
    Parse_c.parse_program file
    )))
  with 
  | Timeout -> raise Timeout
  | exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file (Common.exn_to_s exn));
    raise exn


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

  | Global v ->
      (* skip extern decl *)
      (match v.v_storage with
      | Extern -> []
      | Static | DefaultStorage ->
          [(Ast.str_of_name v.v_name, E.Global)]
      )

   (* todo: maybe letter, but need to find the real File
    * corresponding to the string, so may need some -I
    *)
  | Include _ -> []
  | Undef _ -> []
  (* do we want them? *)
  | Prototype _ -> []

let rec add_use_edge env (name, kind) =
  let src = env.current in
  let dst = (Ast.str_of_name name, kind) in

  (match () with
  | _ when not (G.has_node src env.g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));
      env.lookup_fails#update src Common2.add1

  (* now handled via G.dupe nodes
   * we skip reference to dupes
   * | _ when Hashtbl.mem env.dupes src ->
   *      env.lookup_fails#update src Common.add1
   * 
   * | _ when Hashtbl.mem env.dupes dst -> 
   * env.lookup_fails#update dst Common.add1
   *)

  | _ when G.has_node dst env.g -> 
      G.add_edge (src, dst) G.Use env.g

  | _ -> 
      (match kind with
      (* sometimes people don't use uppercase for macros *)
      | E.Global ->
          add_use_edge env (name, E.Constant)
      | E.Function ->
          add_use_edge env (name, E.Macro)

      | _ ->
          (* recorrect dst, put back Global and Function *)
          let kind_original =
            match kind with
            | E.Constant when not (looks_like_macro name) -> E.Global
            | E.Macro when not (looks_like_macro name) -> E.Function
            | _ -> kind
          in
          let dst = (Ast.str_of_name name, kind_original) in
            
          G.add_node dst env.g;
          let parent_target = 
            if Hashtbl.mem env.dupes dst
            then raise Impossible
            else G.not_found
          in
          pr2 (spf "PB: lookup fail on %s (in %s)" 
                       (G.string_of_node dst) (G.string_of_node src));

          env.g +> G.add_edge (parent_target, dst) G.Has;
          env.g +> G.add_edge (src, dst) G.Use;
          env.lookup_fails#update dst Common2.add1
      )
  )

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

let extract_defs ~g ~dupes ~ast ~readable =
  let dir = Common2.dirname readable in
  G.create_intermediate_directories_if_not_present g dir;
  g +> G.add_node (readable, E.File);
  g +> G.add_edge ((dir, E.Dir), (readable, E.File))  G.Has;

  ast +> List.iter (fun e ->
    let nodes = nodes_of_toplevel e in

    nodes +> List.iter (fun node ->

      (* todo: if StructDef or EnumDef then
       * can have nested Has links to add
       *)
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

let rec extract_uses ~g ~ast ~dupes ~readable ~lookup_fails =
  let env = {
    current = (readable, E.File);
    g;
    dupes; lookup_fails;
    params_locals = [];
  }
  in
  toplevels env ast

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env x =
  let xs = nodes_of_toplevel x in
  let env = 
    match xs with
    | [] -> env
    | [x]  
    (* can happen for EnumDef *)
    | x::_
      -> 
        (* can happen for main() which will be dupes in which case
         * it's better to keep current as the current File so
         * at least we will avoid some fail lookup.
         *)
        if Hashtbl.mem env.dupes x
        then env
        else { env with current = x }
        
  in
  match x with
  | Define (name, v) ->
      define_body env v
  | Macro (name, params, body) -> 
      let xs =  params +> List.map Ast.str_of_name in
      let env = { env with params_locals = xs } in
      define_body env body
  | FuncDef def -> 
      let (ret, params) = def.f_type in
      type_ env ret;
      parameters env params;
      let xs = params +> Common.map_filter (fun x -> 
        (match x.p_name with None -> None  | Some x -> Some (Ast.str_of_name x))
      ) in
      let env = { env with params_locals = xs } in
      stmts env def.f_body

  | StructDef { s_name = n; s_kind = _kind; s_flds = flds } -> 
      flds +> List.iter (fun { fld_name = n; fld_type = t; } ->
        type_ env t
      )
        
  | EnumDef (name, xs) ->
      xs +> List.iter (fun (name, eopt) ->
        Common2.opt (expr env) eopt
      )

  | TypeDef (name, t) -> type_ env t

  | Global x -> 
      (match x with
        { v_name = n; v_type = t; v_storage = _; v_init = eopt } ->
          (* env.params_locals <- (Ast.str_of_name n)::env.params_locals; *)
          type_ env t;
          Common2.opt (expr env) eopt
      )

  (* less: should analyze if s has the form "..." and not <> and
   * build appropriate link?
   *)
  | Include _ -> ()
  | Undef _ -> ()
 
  (* less: do we want them? *)
  | Prototype def -> ()

and toplevels env xs = List.iter (toplevel env) xs

and define_body env v =
  match v with
  | CppExpr e -> expr env e
  | CppStmt st -> stmt env st
  | CppEmpty -> ()

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)
and stmt env = function
  | Expr e -> expr env e
  | Block xs -> stmts env xs
  | Asm e -> exprs env e
  | If (e, st1, st2) ->
      expr env e;
      stmts env [st1; st2]
  | Switch (e, xs) ->
      expr env e;
      cases env xs
  | While (e, st) | DoWhile (st, e) -> 
      expr env e;
      stmt env st
  | For (e1, e2, e3, st) ->
      Common2.opt (expr env) e1;
      Common2.opt (expr env) e2;
      Common2.opt (expr env) e3;
      stmt env st
  | Return eopt ->
      Common2.opt (expr env) eopt;
  | Continue | Break -> ()
  | Label (_name, st) ->
      stmt env st
  | Goto name ->
      ()

  | Vars xs ->
      xs +> List.iter (fun 
        { v_name = n; v_type = t; v_storage = _; v_init = eopt } ->
          env.params_locals <- (Ast.str_of_name n)::env.params_locals;
          type_ env t;
          Common2.opt (expr env) eopt
      )

 and case env = function
   | Case (e, xs) -> 
       expr env e;
       stmts env xs
   | Default xs ->
       stmts env xs

and stmts env xs = List.iter (stmt env) xs

and cases env xs = List.iter (case env) xs

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
      let s = Ast.str_of_name name in
      (match () with
      | _ when List.mem s env.params_locals -> ()
      | _ when looks_like_macro name ->
          add_use_edge env (name, E.Constant)
      | _ ->
          add_use_edge env (name, E.Global)  
      )

  | Call (e, es) -> 
      (match e with
      | Id name ->
          if looks_like_macro name
          then add_use_edge env (name, E.Macro)
          else add_use_edge env (name, E.Function)
      | _ -> expr env e
      );
      exprs env es
  | Assign (_, e1, e2) -> exprs env [e1; e2]
  | ArrayAccess (e1, e2) -> exprs env [e1; e2]
  (* todo: determine type of e and make appropriate use link *)
  | RecordAccess (e, name) -> expr env e

  | Cast (t, e) -> 
      type_ env t;
      expr env e

  | Postfix (e, _op) | Infix (e, _op) -> expr env e
  | Unary (e, op) -> expr env e
  | Binary (e1, op, e2) -> exprs env [e1;e2]

  | CondExpr (e1, e2, e3) -> exprs env [e1;e2;e3]
  | Sequence (e1, e2) -> exprs env [e1;e2]

  | InitList xs -> exprs env xs

  | SizeOf x ->
      (match x with
      | Left e -> expr env e
      | Right t -> type_ env t
      )
  | GccConstructor (t, e) ->
      type_ env t;
      expr env e


and exprs env xs = List.iter (expr env) xs

(* ---------------------------------------------------------------------- *)
(* Types *)
(* ---------------------------------------------------------------------- *)
and type_ env x =
  match x with
  | TBase _ -> ()
  | TPointer t | TArray t -> type_ env t
  | TFunction (tret, xs) ->
      type_ env tret;
      parameters env xs
  | TStructName (_kind, name) ->
      add_use_edge env (name, E.Class E.RegularClass)
  | TEnumName (name) | TTypeName name ->
      add_use_edge env (name, E.Type)

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

and parameter env { p_type = t; p_name = _} =
  type_ env t
and parameters env xs = 
  List.iter (parameter env) xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) root files =
  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  let dupes = Hashtbl.create 101 in
  files +> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let readable = Common.readable ~root file in
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
  let lookup_fails = Common2.hash_with_default (fun () -> 0) in
  files +> Console.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.readable ~root file in
     let ast = parse ~show_parse_error:false file in
     extract_uses ~g ~dupes ~ast ~readable ~lookup_fails;
   ));
(* make less sense now that have G.not_found
  lookup_fails#to_list +> Common.sort_by_val_highfirst +> Common.take_safe 20
  +> List.iter (fun (n, cnt) ->
    pr2 (spf "LOOKUP FAIL: %s (%d)%s" (G.string_of_node n) cnt
            (if Hashtbl.mem dupes n then "(DUPE)" else ""))
  );
*)
  g
