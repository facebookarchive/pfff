(* Jeff Morrison
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

open Ast_js
module J = Json_type

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Convert from the JSON AST produced by esprima to
 * the OCaml AST in Ast_js.
 * 
 * https://github.com/facebook/esprima/tree/fb-harmony
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fake_tok() =
 { Parse_info.
   token = Parse_info.FakeTokStr ("Fake", None);
   transfo = Parse_info.NoTransfo;
 }

let t = fake_tok()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rangeloc rangeProp locProp = match rangeProp, locProp with
  | ("range", J.Array range), ("loc", J.Object loc) -> ()
  | _ -> raise Todo(*J.Object [rangeProp; locProp]*)

let rec convert json =
  match json with
  | J.Object
    [
      ("type", J.String "Program");
      ("body", J.Array body);
      rangeProp;
      locProp; 
    ] ->
      List.map toplevel body
  | _ -> raise Todo

and toplevel json =
  match json with
  | J.Object node ->
      (match node with
      | [
          ("type", J.String "FunctionDeclaration");
          ("id", J.Object([
            ("type", J.String "Identifier");
            ("name", J.String name);
            nameRangeProp;
            nameLocProp;
          ]));
          ("params", J.Array params);
          ("defaults", J.Array defaults);
          ("body", J.Object([
            ("type", J.String "BlockStatement");
            ("body", J.Array body);
            bodyRangeProp;
            locRangeProp;
          ]));
          ("rest", J.Null);
          ("generator", J.Bool isGenerator);
          ("expression", J.Bool wtfIsThis);
          rangeProp;
          locProp;
        ] -> FunDecl({
          f_tok=Some(t);
          f_name=Some((name, t));
          f_params=(t, [] (* TODO *), t);
          f_return_type=None;
          f_body=(t, List.map toplevel body, t);
        })
      | _ -> St(stmnt node))
  | _ -> raise Todo

and stmnt node =
  match node with
  | [
      ("type", J.String "ExpressionStatement");
      ("expression", J.Object expr);
      rangeProp;
      locProp;
    ] -> ExprStmt(expression expr, Some(t))
  | [
      ("type", J.String "EmptyStatement");
      rangeProp;
      locProp;
    ] -> Nop(Some(t))
  | [
      ("type", J.String "BlockStatement");
      ("body", J.Array body);
      rangeProp;
      locProp;
    ] -> Block(t, toplevels body, t)
  | [
      ("type", J.String "IfStatement");
      ("test", J.Object testExpr);
      ("consequent", J.Object consequentBlock);
      ("alternate", J.Object alternateBlock);
      rangeProp;
      locProp;
    ] -> If(
      t,
      (t, expression testExpr, t),
      stmnt consequentBlock,
      Some(t, stmnt alternateBlock)
    )
  | [
      ("type", J.String "LabeledStatement");
      ("label", J.Object([
        ("type", J.String "Identifier");
        ("name", J.String name);
        identRangeProp;
        identLocProp;
      ]));
      ("body", J.Object body);
      rangeProp;
      locProp;
    ] -> Labeled((name, t), t, stmnt body)
  | [
      ("type", J.String "SwitchStatement");
      ("discriminant", J.Object discrim);
      ("cases", J.Array cases);
      rangeProp;
      locProp;
    ] -> Switch(
      t,
      (t, expression discrim, t),
      (t, List.map case_clause cases, t)
    )
  | [
      ("type", J.String "BreakStatement");
      ("label", J.Object([
        ("type", J.String "Identifier");
        ("name", J.String labelStr);
        labelRangeProp;
        labelLocProp;
      ]));
      rangeProp;
      locProp;
    ] -> Break(t, Some((labelStr, t)), Some(t))
  | [
      ("type", J.String "BreakStatement");
      ("label", J.Null);
      rangeProp;
      locProp;
    ] -> Break(t, None, Some(t))
  | [
      ("type", J.String "WhileStatement");
      ("test", J.Object testExpr);
      ("body", J.Object body);
      rangeProp;
      locProp;
    ] -> While(
      t, 
      (t, expression testExpr, t),
      stmnt body
    )
  | [
      ("type", J.String "ContinueStatement");
      ("label", J.Object([
        ("type", J.String "Identifier");
        ("name", J.String labelName);
        labelRangeProp;
        labelLocProp;
      ]));
      rangeProp;
      locProp;
    ] -> Continue(t, Some((labelName, t)), Some(t))
  | [
      ("type", J.String "ContinueStatement");
      ("label", J.Null);
      rangeProp;
      locProp;
    ] -> Continue(t, None, Some(t))
  | [
      ("type", J.String "WithStatement");
      ("object", J.Object obj);
      ("body", J.Object body);
      rangeProp;
      locProp;
    ] -> With(t, (t, expression obj, t), stmnt body)
  | [
      ("type", J.String "ReturnStatement");
      ("argument", J.Object arg);
      rangeProp;
      locProp;
    ] -> Return(t, Some(expression arg), Some(t))
  | [
      ("type", J.String "ReturnStatement");
      ("argument", J.Null);
      rangeProp;
      locProp;
    ] -> Return(t, None, Some(t))
  | [
      ("type", J.String "ThrowStatement");
      ("argument", J.Object arg);
      rangeProp;
      locProp;
    ] -> Throw(t, expression arg, Some(t))
  | [
      ("type", J.String "TryStatement");
      ("block", J.Object tryBlock);
      ("guardedHandlers", J.Array guardedHandlers);
      ("handlers", J.Array([
        catch
      ]));
      ("finalizer", J.Null);
      rangeProp;
      locProp;
    ] -> Try(
      t, 
      stmnt tryBlock, 
      Some(catch_clause catch),
      None
    )
  | [
      ("type", J.String "TryStatement");
      ("block", J.Object tryBlock);
      ("guardedHandlers", J.Array guardedHandlers);
      ("handlers", J.Array([
        catch
      ]));
      ("finalizer", J.Object finallyBlock);
      rangeProp;
      locProp;
    ] -> Try(
      t, 
      stmnt tryBlock, 
      Some(catch_clause catch),
      Some((t, stmnt finallyBlock))
    )
  | [
      ("type", J.String "TryStatement");
      ("block", J.Object tryBlock);
      ("guardedHandlers", J.Array guardedHandlers);
      ("handlers", J.Array([]));
      ("finalizer", J.Object finallyBlock);
      rangeProp;
      locProp;
    ] -> Try(
      t, 
      stmnt tryBlock, 
      None,
      Some((t, stmnt finallyBlock))
    )
  | [
      ("type", J.String "DoWhileStatement");
      ("body", J.Object body);
      ("test", J.Object test);
      rangeProp;
      locProp;
    ] -> Do(t, stmnt body, t, (t, expression test, t), Some(t))
  | [
      ("type", J.String "ForStatement");
      ("init", init);
      ("test", test);
      ("update", update);
      ("body", J.Object body);
      rangeProp;
      locProp;
    ] -> For(
      t, t, (* `for (` *)
      for_init init, t,
      expr_or_none test, t,
      expr_or_none update, t,
      stmnt body
    )
  | _ -> raise Todo

and for_init init = 
  match init with
  | J.Object [
      ("type", J.String "VariableDeclaration");
      ("declarations", J.Array decls);
      ("kind", kind);
      rangeProp;
      locProp;
    ] -> None (* TODO *)
  | J.Object initNode -> Some(LHS(expression initNode))
  | J.Null -> None
  | _ -> raise Todo

and expr_or_none node =
  match node with
  | J.Object xs -> Some(expression xs)
  | J.Null -> None
  | _ -> raise Todo

and catch_clause catch =
  match catch with
  | J.Object node -> (match node with
    | [
        ("type", J.String "CatchClause");
        ("param", J.Object([
          ("type", J.String "Identifier");
          ("name", J.String paramArg);
          nameRangeProp;
          nameLocProp;
        ]));
        ("body", J.Object body);
        rangeProp;
        locProp;
      ] -> (t, (t, (paramArg, t), t), stmnt body)
    | _ -> raise Todo)
  | _ -> raise Todo

and case_clause case =
  match case with 
  | J.Object xs -> (match xs with
    | [
        ("type", J.String "SwitchCase");
        ("test", J.Object test);
        ("consequent", J.Array conseq);
        rangeProp;
        locProp;
      ] -> Case(
        t, 
        expression test, 
        t, 
        toplevels conseq
      )
    | _ -> raise Todo)
  | _ -> raise Todo

and expression node =
  match node with
  | [
      ("type", J.String "Identifier");
      ("name", J.String identname);
      rangeProp;
      locProp;
    ] -> V(identname, t)
  | [
      ("type", J.String "Literal");
      ("value", J.Int value);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> L(Num(raw, t))
  | [
      ("type", J.String "Literal");
      ("value", J.String value);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> L(String(value, t));
  | [
      ("type", J.String "Literal");
      ("value", J.Bool value);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> L(Bool(value, t))
  | _ -> raise Todo

and parameters xs =
  match xs with
  | [] -> []
  | _ -> raise Todo

and toplevels xs =
  match xs with
  | [] -> []
  | x::xs -> toplevel x :: toplevels xs
