(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_php

module Ast = Ast_php
module R = Runtime_php
module B = Lib_builtins_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Why ? There is already the Zend PHP interpreter and HPHPi.
 * 
 * It can help me learn the complex PHP semantic which can be useful
 * later when designing bug-finding tools for PHP.
 * Also I never really wrote some interpreters/JIT/compilers so I have
 * no gut feelings about their relative cost. How fast can be a naive
 * PHP interpreter in OCaml vs the C Zend version ? vs a naive JIT ?
 * 
 * references:
 *  - the art of the interpreter ?
 *  - PL zoo, http://andrej.com/plzoo/
 *  - MiniCaml by ejiro sumii
 *  - The book by leroy on OCaml contains an interpreter for pascal
 *  - SICP, EOPL, etc
 * 
 * related work: Lua-ML
 *)

(*****************************************************************************)
(* types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lookup_env s h = 
  Hashtbl.find h s

(* todo: maybe clearer to make i_stmt return a either3 ?
*)
exception ReturnExn of R.rval

let is_true_for_cond_v v = 
  match v with
  | R.RBool b -> b

  | _ -> raise Todo


let arith_cmp cmp a b = 
  let b = 
    match a, b with
    | R.RInt a, R.RInt b -> cmp a b
    | _ -> raise Todo
  in
  R.RBool b

let arith_int op a b = 
  let i = 
    match a, b with
    | R.RInt a, R.RInt b -> op a b
    | _ -> raise Todo
  in
  R.RInt i


(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* semi global technique *)
let env = R.default_env ()

let rec interpret_file file =
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  interpret ast

and interpret asts = 
  asts |> List.iter (fun ast -> 
    match ast with
    | StmtList xs ->
        xs |> List.iter (fun x -> 
          i_stmt x |> ignore;
        );
    | FuncDef def ->
        let s = Ast.name def.f_name in
        Hashtbl.add env.R.functions s def
    | ClassDef def ->
        raise Todo
    | InterfaceDef def ->
        raise Todo


    | Halt _ -> raise Todo
    | NotParsedCorrectly _ -> 
        pr2 "NotParsedCorrectly";
    | FinalDef _ -> ()
  )

and i_stmt st = 
  match st with
  | ExprStmt ((e, tok)) -> 
      raise Todo
  | EmptyStmt tok -> 
      raise Todo
  | Block xs_brace -> 
      let xs = Ast.unbrace xs_brace in
      i_stmt_and_defs xs
  | If ((tok, eparen, stmt, elseif, elseopt)) ->
      let e = Ast.unparen eparen in
      let v = i_expr e in
      if is_true_for_cond_v v
      then
        i_stmt stmt
      else 
        (match elseif with
        | [] ->
            (match elseopt with
            | None -> 
                R.RNull (* ?? *)
            | Some (tok, elsest) ->
                i_stmt elsest
            )
        | x::xs ->
            raise Todo
        )

  | IfColon ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      raise Todo
  | While ((v1, v2, v3)) ->
      raise Todo
  | Do ((v1, v2, v3, v4, v5)) ->
      raise Todo
  | For ((v1, v2, v3, v4, v5, v6, v7, v8, v9)) ->
      raise Todo
  | Switch ((v1, v2, v3)) ->
      raise Todo
  | Foreach ((v1, v2, v3, v4, v5, v6, v7, v8)) ->
      raise Todo
  | Break ((v1, v2, v3)) ->
      raise Todo
  | Continue ((v1, v2, v3)) ->
      raise Todo
  | Return ((t1, eopt, t2)) ->
      (match eopt with
      | None -> R.RNull (* ? *)
      | Some e -> 
          let v = i_expr e in
          raise (ReturnExn v)

      )
  | Throw ((v1, v2, v3)) ->
      raise Todo
  | Try ((v1, v2, v3, v4)) ->
      raise Todo
  | Echo ((v1, es, v3)) ->
      (* order of evaluation ? left to right ? *)
      let vs = es |> List.map i_expr in
      vs |> List.iter (fun v ->
        B.echo v
      );
      R.RNull (* ? *)

  | InlineHtml v1 -> 
      raise Todo
  | Globals ((v1, v2, v3)) ->
      raise Todo
  | StaticVars ((v1, v2, v3)) ->
      raise Todo
  | Ast.Use ((v1, v2, v3)) ->
      raise Todo
  | Unset ((v1, v2, v3)) ->
      raise Todo
  | Declare ((v1, v2, v3)) -> 
      raise Todo

and i_stmt_and_def x = 
  match x with
  | Stmt st -> i_stmt st
  | FuncDefNested _ -> 
      raise Todo
  | ClassDefNested _ -> 
      raise Todo
  | InterfaceDefNested _ -> 
      raise Todo

(* the semantic of a set of statements is the value of a 'return' 
 * if there is one, or the value of the last executed statements 
 * (e.g. '1;'), or null ?
 *)
and i_stmt_and_defs xs = 
  match xs with
  | [] -> 
      (* return null ? *)
      raise Todo
  | x::xs ->
      try
        let v = i_stmt_and_def x in
        (match xs with
        | [] -> 
            v
        | _::_ ->
            i_stmt_and_defs xs
        )
      with ReturnExn v ->
        v

and i_expr e = 
  match Ast.untype e with
  | Lvalue v ->
      let v = i_lvalue v in
      v.R.v

  | Scalar v ->
      i_scalar v
  | Assign ((v1, v2, v3)) ->
      raise Todo
  | AssignRef ((v1, v2, v3, v4)) ->
      raise Todo
  | AssignNew ((v1, v2, v3, v4, v5, v6)) ->
      raise Todo
  | AssignOp ((v1, v2, v3)) ->
      raise Todo
  | Postfix ((v1, v2)) ->
      raise Todo
  | Infix ((v1, v2)) ->
      raise Todo
  | Binary ((a, op, b)) ->
      (match Ast.unwrap op with
      | Arith op -> 
          let va = i_expr a in
          let vb = i_expr b in
          (match op with
          | Plus -> arith_int (+) va vb
          | Minus -> arith_int (-) va vb
          | Mul -> arith_int ( * ) va vb
          | Div -> arith_int ( / ) va vb
          | Mod -> raise Todo
              
          | DecLeft -> raise Todo
          | DecRight -> raise Todo

          | And -> raise Todo
          | Or -> raise Todo
          | Xor -> raise Todo
          )
      | Logical op ->
          let va = i_expr a in

          (match op with
          | AndBool | OrBool ->
              raise Todo
          | _ ->
              let vb = i_expr b in
              
              (match op with
              | Inf -> arith_cmp (<) va vb
              | Sup -> arith_cmp (>) va vb
              | InfEq -> arith_cmp (<=) va vb
              | SupEq -> arith_cmp (>=) va vb

              | Eq -> raise Todo
              | NotEq -> raise Todo

              | Identical -> raise Todo
              | NotIdentical -> raise Todo

              | AndLog -> raise Todo
              | OrLog -> raise Todo
              | XorLog -> raise Todo

              | AndBool | OrBool -> raise Impossible
              )
          )

      | BinaryConcat ->
          raise Todo
      )

  | Unary ((v1, v2)) ->
      raise Todo
  | CondExpr ((v1, v2, v3, v4, v5)) ->
      raise Todo
  | ConsList ((v1, v2, v3, v4)) ->
      raise Todo
  | ConsArray ((v1, v2)) ->
      raise Todo
  | New ((v1, v2, v3)) ->
      raise Todo
  | Clone ((v1, v2)) ->
      raise Todo
  | InstanceOf ((v1, v2, v3)) ->
      raise Todo
  | Cast ((v1, v2)) ->
      raise Todo
  | CastUnset ((v1, v2)) ->
      raise Todo
  | Exit ((v1, v2)) ->
      raise Todo
  | At ((v1, v2)) ->
      raise Todo
  | Print ((v1, v2)) ->
      raise Todo
  | BackQuote ((v1, v2, v3)) ->
      raise Todo
  | Include ((v1, v2)) ->
      raise Todo
  | IncludeOnce ((v1, v2)) ->
      raise Todo
  | Require ((v1, v2)) ->
      raise Todo
  | RequireOnce ((v1, v2)) ->
      raise Todo
  | Empty ((v1, v2)) ->
      raise Todo
  | Isset ((v1, v2)) ->
      raise Todo
  | Eval ((v1, v2)) ->
      raise Todo
  | ParenExpr v1 -> 
      raise Todo


  | Lambda _ ->
      raise Todo
  | EDots _ ->
      raise Todo

and i_scalar x = 
  match x with
  | Constant cst -> 
      i_constant cst
  | ClassConstant (qu, name) -> 
      raise Todo

  | Guil _ ->
      raise Todo
  | HereDoc _ ->
      raise Todo

and i_constant x = 
  match x with
  | Int s ->
      (* can raise exn *)
      let i = int_of_string (Ast.unwrap s) in
      R.RInt i
  | Double s ->
      let f = float_of_string (Ast.unwrap s) in
      R.RFloat f

  | String s ->
      (* TODO interpret string content ? *)
      R.RString (Ast.unwrap s)

  | CName name ->
      raise Todo


  | PreProcess _ ->
      raise Todo

  | XdebugClass _ 
  | XdebugResource ->
      failwith "impossible: xdebug constructions"

and i_lvalue x = 
  match Ast.untype x with
  | Var ((dname, scope)) ->
      (match !scope with
      | Scope_php.NoScope ->
          pr2_once "NoScope, have called annotate_scope ?";
          
          let s = Ast.dname dname in
          let v  = lookup_env s env.R.variables in
          v

      | Scope_php.Param | Scope_php.Local ->
          let s = Ast.dname dname in
          let v  = lookup_env s env.R.variables in
          v
      | Scope_php.Global ->
          raise Todo
      )

  | VArrayAccess ((v1, v2)) ->
      raise Todo
  | VBrace ((v1, v2)) -> 
      raise Todo
  | VBraceAccess ((v1, v2)) ->
      raise Todo
  | Indirect ((v1, v2)) ->
      raise Todo
  | FunCallSimple ((qu_opt, name, args)) ->
      qu_opt |> Common.do_option (fun _ -> raise Todo);
      
      let s = Ast.name name in 
      let func = lookup_env s env.R.functions in
      let args = Ast.unparen args in 
      
      apply_funcall func args
      
  | FunCallVar ((v1, v2, v3)) ->
      raise Todo
  | VQualifier ((v1, v2)) ->
      raise Todo

  | MethodCallSimple ((v1, v2, v3, v4)) ->
      raise Todo
  | ObjAccessSimple ((v1, v2, v3)) ->
      raise Todo

  | ObjAccess ((v1, v2)) ->
      raise Todo


and apply_funcall def args = 
  def.f_ref |> Common.do_option (fun _ -> failwith "TODO: f_ref");
  let params = Ast.unparen def.f_params in
  
  if List.length params <> List.length args 
  then failwith "TODO: not same number of arguments";

  (* adjusting the environment with the new frame *)
  Common.zip params args |> List.iter (fun (p,a) ->
    p.p_default |> Common.do_option (fun _ -> failwith "TODO: p_default");
    p.p_ref |> Common.do_option (fun _ -> failwith "TODO: p_ref");

    let v = 
      match a with
      | Arg e -> i_expr e
      | ArgRef _ -> raise Todo
    in
    let s = Ast.dname p.p_name in
    Hashtbl.add env.R.variables s { R.v = v };
  );

  let body = Ast.unbrace def.f_body in 
  let ret = i_stmt_and_defs body in

  (* unset the vars in the environment *)
  params |> List.iter (fun p ->
    let s = Ast.dname p.p_name in
    Hashtbl.remove env.R.variables s
  );
  {R.v = ret }
  
