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

(* for fields *)
open Ast_php

module A = Ast_php
module B = Ast_mini_php

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception NotMiniPhp

(*****************************************************************************)
(* Conversion *)
(*****************************************************************************)

let noType () = { B.t = [B.TUnknown] }
let mk_e e = (e, noType ())


let rec stmt st = 
  match st with
  | Echo (_, exprs, _) ->
      (match A.uncomma exprs with
      | [e] -> B.Echo (expr e)
      | _ -> raise NotMiniPhp
      )
  | If(v1, v2, v3, v4, v5) ->
      if not (null v4) then raise NotMiniPhp;

      let e = expr (A.unparen v2) in
      let st1 = stmt v3 in
      let st2 = v5 +> Common.fmap  (fun (tok, st) -> stmt st) in
      B.If(e, st1, st2)
  | While(_, v2, (SingleStmt st)) ->
      let e = expr (A.unparen v2) in
      let st = stmt st in
      B.While (e, st)

  | Block(xs) -> 
      B.Block (xs +> A.unbrace +> List.map stmt_and_def)

  | ExprStmt(e,_) -> B.ExprStmt (expr e)
  | Return(_, eopt, _) -> B.Return (eopt +> Common.fmap expr)

  | _ -> raise NotMiniPhp


and stmt_and_def stdef =
  match stdef with
  | Stmt st -> stmt st
  | _ -> raise NotMiniPhp

and expr e = 
  let ebis = fst e in
  match ebis with
  | Sc scalar ->
      scalar_to_expr scalar
  | Lv var ->
      variable_to_expr var

  | Assign (var, _, e) ->
      mk_e (B.Assign (variable_to_expr var, expr e))

  | Binary(e1, (op,_), e2) ->
      let e1 = expr e1 in
      let e2 = expr e2 in
      mk_e (B.Binary (e1, op, e2))

  | ParenExpr(eparen) -> 
      expr (A.unparen eparen)


  | _ -> raise NotMiniPhp

and scalar_to_expr scalar =
  match scalar with
  | C cst ->
      constant_to_expr cst

  | _ -> raise NotMiniPhp

and constant_to_expr cst =
  let cst' = 
  match cst with
  | Int (s,_) -> B.Number s
  | Double (s,_) -> B.Number s
  | String (s,_) -> B.String s

  | CName name ->
      let s = A.name name in
      (match s with
      | "null" -> B.Null
      (* TODO a Bool type ? *)
      | "true" -> B.Bool true
      | "false" -> B.Bool false

      | _ -> raise NotMiniPhp
      )

  | _ -> raise NotMiniPhp
  in
  mk_e cst'

and variable_to_expr variable = 
  match A.untype variable with
  | FunCallSimple (name, args) ->
      let args' = A.unparen args +> A.uncomma +> List.map argument in

      let s = A.name name in
      mk_e (B.Funcall (s, args'))

  | Var (dname, _) ->
      let s = A.dname dname in
      mk_e (B.Var s)

  | VArrayAccess (var, eopt_bracket) ->
      let eopt = A.unbracket eopt_bracket in
      let e' = 
        match eopt with
        | None -> raise NotMiniPhp
        | Some e -> expr e
      in
      let var' = variable_to_expr var in
      mk_e (B.ArrayAccess (var', e'))

  | _ -> raise NotMiniPhp


and argument arg = 
  match arg with
  | Arg e -> expr e
  | ArgRef _ -> raise NotMiniPhp
    

and static_scalar_to_expr static_scalar =
  match static_scalar with
  | StaticConstant cst -> constant_to_expr cst
  | _ -> raise NotMiniPhp

and ast_to_mini asts =
  asts +> Common.map_filter (function

  | A.FuncDef def ->
      let s = A.name def.f_name in
      let params = 
        def.f_params +> A.unparen +> A.uncomma_dots +> List.map (fun param ->
          let s = A.dname param.p_name in
          let eopt = 
            param.p_default +> Common.fmap (fun (tokeq, static_scalar) ->
              static_scalar_to_expr static_scalar
            )
          in
          s, eopt
        )
      in
      let stmts = def.f_body +> A.unbrace +> List.map stmt_and_def in
      Some (B.FuncDef (s, params, stmts))
      

  | A.StmtList xs ->
      Some (B.StmtList (xs +> List.map stmt))

  | A.FinalDef _ -> None

  | _ -> 
      raise NotMiniPhp
  )  


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(* just reusing regular php parser and transforming it. Julia's tech *)
let parse file = 
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast_to_mini ast

