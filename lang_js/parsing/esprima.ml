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

let rec comma_sep_list mapper elems = 
  match elems with
  | [] -> []
  | J.Object x::xs -> Left(mapper x) :: comma_sep_list mapper xs
  | _ -> raise Todo

let resolve_assign_operator operator =
  match operator with
  | "=" -> A_eq
  | "*=" -> A_mul
  | "/=" -> A_div
  | "%=" -> A_mod
  | "+=" -> A_add
  | "-=" -> A_sub
  | "<<=" -> A_lsl
  | ">>=" -> A_lsr
  | ">>>=" -> A_asr
  | "&=" -> A_and
  | "^=" -> A_xor
  | "|=" -> A_or
  | _ -> raise Todo

let resolve_unary_operator operator =
  match operator with
  | "!" -> U_not
  | "-" -> U_minus
  | "+" -> U_plus
  | "~" -> U_bitnot
  | "typeof" -> U_typeof
  | "void" -> U_void
  | "delete" -> U_delete
  | _ -> raise Todo

let resolve_update_operator operator is_prefix =
  match operator with
  | "++" -> 
      if is_prefix then U_pre_increment else U_post_increment
  | "--" ->
      if is_prefix then U_pre_decrement else U_post_decrement
  | _ -> raise Todo

let resolve_binary_operator operator =
  match operator with
  | "==" -> B_equal
  | "!=" -> B_notequal
  | "===" -> B_physequal
  | "!==" -> B_physnotequal
  | "<" -> B_lt
  | "<=" -> B_le
  | ">" -> B_gt
  | ">=" -> B_ge
  | "<<" -> B_lsl
  | ">>" -> B_lsr
  | ">>>" -> B_asr
  | "+" -> B_add
  | "-" -> B_sub
  | "*" -> B_mul
  | "/" -> B_div
  | "%" -> B_mod
  | "|" -> B_or
  | "^" -> B_bitxor
  | "&" -> B_bitand
  | "in" -> B_in
  | "instanceof" -> B_instanceof
  | _ -> raise Todo

let resolve_logical_operator operator =
  match operator with
  | "||" -> B_or
  | "&&" -> B_and
  | _ -> raise Todo

let resolve_literal_prop_name value raw =
  match value with
  | J.Int value -> PN_Num((raw, t))
  | J.Float value -> PN_Num((raw, t))
  | J.String value -> PN_String((value, t))
  | _ -> raise Todo

let resolve_prop_name key =
  match key with
  | [
      ("type", J.String "Identifier");
      ("name", J.String name);
      rangeProp;
      locProp;
    ] -> PN_String((name, t))
  | [
      ("type", J.String "Literal");
      ("value", value);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> (
      match value with
      | J.Int value -> PN_Num((raw, t))
      | J.Float value -> PN_Num((raw, t))
      | J.String value -> PN_String((value, t))
      | _ -> raise Todo
    )
  | _ -> raise Todo

let func_param param =
  match param with
  | [
      ("type", J.String "Identifier");
      ("name", J.String name);
      rangeProp;
      locProp;
    ] -> {p_name=(name, t); p_type=None (* TODO *)}
  | _ -> raise Todo


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
          f_params=(t, comma_sep_list func_param params, t);
          f_return_type=None; (* TODO *)
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
      ("type", J.String "IfStatement");
      ("test", J.Object testExpr);
      ("consequent", J.Object consequentBlock);
      ("alternate", J.Null);
      rangeProp;
      locProp;
    ] -> If(
      t,
      (t, expression testExpr, t),
      stmnt consequentBlock,
      None
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
      ("label", label);
      rangeProp;
      locProp;
    ] -> Break(t, ident_name_or_none label, Some(t))
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
      ("label", label);
      rangeProp;
      locProp;
    ] -> Continue(t, ident_name_or_none label, Some(t))
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
      (if init == J.Null then None else Some(for_init init)), t,
      expr_or_none test, t,
      expr_or_none update, t,
      stmnt body
    )
  | [
      ("type", J.String "ForInStatement");
      ("left", left);
      ("right", J.Object right);
      ("body", J.Object body);
      ("each", J.Bool spidermonkeyEach);
      rangeProp;
      locProp;
    ] -> ForIn(
      t, t, (* `for (` *)
      for_init left, t,
      expression right, t,
      stmnt body
    )
  | [
      ("type", J.String "VariableDeclaration");
      ("declarations", J.Array decls);
      ("kind", kind);
      rangeProp;
      locProp;
    ] -> Variable(t, comma_sep_list var_declarator decls, Some(t))
  | [
      ("type", J.String "DebuggerStatement");
      rangeProp;
      locProp;
    ] -> ExprStmt(V(("debugger", t)), Some(t))
  | _ -> raise Todo

and for_init init = 
  match init with
  | J.Object [
      ("type", J.String "VariableDeclaration");
      ("declarations", J.Array decls);
      ("kind", kind);
      rangeProp;
      locProp;
    ] -> Vars(t, comma_sep_list var_declarator decls)
  | J.Object initNode -> LHS(expression initNode)
  | _ -> raise Todo

and var_declarator decl =
  match decl with
  | [
      ("type", J.String "VariableDeclarator");
      ("id", J.Object([
        ("type", J.String "Identifier");
        ("name", J.String name);
        identRangeProp;
        identLocProp;
      ]));
      ("init", init);
      rangeProp;
      locProp;
    ] -> {
      v_name=(name, t);
      v_init=(match init with 
              | J.Null -> None
              | J.Object init -> Some(t, expression init)
              | _ -> raise Todo);
      v_type=None; (* TODO *)
    };
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
    | [
        ("type", J.String "SwitchCase");
        ("test", J.Null);
        ("consequent", J.Array conseq);
        rangeProp;
        locProp;
      ] -> Default(t, t, toplevels conseq)
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
      ("value", J.Float value);
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
  | [
      ("type", J.String "Literal");
      ("value", J.Object value);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> L(Regexp(raw, t))
  | [
      ("type", J.String "Literal");
      ("value", J.Null);
      ("raw", J.String raw);
      rangeProp;
      locProp;
    ] -> L(Null(t))
  | [
      ("type", J.String "ThisExpression");
      rangeProp;
      locProp;
    ] -> This(t)
  | [
      ("type", J.String "ArrayExpression");
      ("elements", J.Array elems);
      rangeProp;
      locProp;
    ] -> Array(t, comma_sep_list expression elems, t)
  | [
      ("type", J.String "AssignmentExpression");
      ("operator", J.String operator);
      ("left", J.Object left);
      ("right", J.Object right);
      rangeProp;
      locProp;
    ] -> Assign(
      expression left,
      (resolve_assign_operator operator, t),
      expression right
    )
  | [
      ("type", J.String "ObjectExpression");
      ("properties", J.Array props);
      rangeProp;
      locProp;
    ] -> Object(t, comma_sep_list object_prop props, t)
  | [
      ("type", J.String "FunctionExpression");
      ("id", id);
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
    ] -> Function({
      f_tok=Some(t);
      f_name=ident_name_or_none id;
      f_params=(t, comma_sep_list func_param params, t);
      f_return_type=None; (* TODO *)
      f_body=(t, List.map toplevel body, t);
    })
  | [
      ("type", J.String "SequenceExpression");
      ("expressions", J.Array exprs);
      rangeProp;
      locProp;
    ] -> recursive_seq exprs
  | [
      ("type", J.String "UnaryExpression");
      ("operator", J.String operator);
      ("argument", J.Object arg);
      rangeProp;
      locProp;
    ] -> U((resolve_unary_operator operator, t), expression arg)
  | [
      ("type", J.String "BinaryExpression");
      ("operator", J.String operator);
      ("left", J.Object left);
      ("right", J.Object right);
      rangeProp;
      locProp;
    ] -> B(
      expression left,
      (resolve_binary_operator operator, t),
      expression right
    )
  | [
      ("type", J.String "UpdateExpression");
      ("operator", J.String operator);
      ("argument", J.Object arg);
      ("prefix", J.Bool is_prefix);
      rangeProp;
      locProp;
    ] -> U((resolve_update_operator operator is_prefix, t), expression arg)
  | [
      ("type", J.String "LogicalExpression");
      ("operator", J.String operator);
      ("left", J.Object left);
      ("right", J.Object right);
      rangeProp;
      locProp;
    ] -> B(
      expression left,
      (resolve_logical_operator operator, t),
      expression right
    )
  | [
      ("type", J.String "ConditionalExpression");
      ("test", J.Object test);
      ("consequent", J.Object consequent);
      ("alternate", J.Object alternate);
      rangeProp;
      locProp;
    ] -> Conditional(
      expression test,
      t,
      expression consequent,
      t,
      expression alternate
    )
  | [
      ("type", J.String "NewExpression");
      ("callee", J.Object callee);
      ("arguments", J.Array args);
      rangeProp;
      locProp;
    ] -> (
      match args with
      | [] -> U((U_new, t), expression callee)
      | _ -> Apply(
        U((U_new, t), expression callee), 
        (t, comma_sep_list expression args, t)
      )
    )
  | [
      ("type", J.String "CallExpression");
      ("callee", J.Object callee);
      ("arguments", J.Array args);
      rangeProp;
      locProp;
    ] -> Apply(
      expression callee, 
      (t, comma_sep_list expression args, t)
    )
  | [
      ("type", J.String "MemberExpression");
      ("computed", J.Bool computed);
      ("object", J.Object obj);
      ("property", J.Object([
        ("type", J.String "Identifier");
        ("name", J.String prop);
        propRangeProp;
        propLocProp;
      ]));
      rangeProp;
      locProp;
    ] -> Period(expression obj, t, (prop, t))
  | _ -> raise Todo

and recursive_seq exprs =
  match exprs with
  | J.Object(x)::xs -> (
      match xs with
      | [J.Object(lastItem)] -> Seq(expression x, t, expression lastItem)
      | _ -> Seq(expression x, t, recursive_seq xs)
    )
  | _ -> raise Todo

and ident_name_or_none node =
  match node with
  | J.Object([
      ("type", J.String "Identifier");
      ("name", J.String name);
      rangeProp;
      locProp;
    ]) -> Some((name, t))
  | J.Null -> None
  | _ -> raise Todo

and object_prop prop =
  match prop with 
  | [
      ("type", J.String "Property");
      ("key", J.Object key);
      ("value", J.Object value);
      ("kind", J.String "init");
      ("method", J.Bool false);
      ("shorthand", J.Bool false);
      rangeProp;
      locProp;
    ] -> (resolve_prop_name key, t, expression value)
  (* One day: Get/Set, shorthand, and method props *)
  | _ -> raise Todo

and toplevels xs =
  match xs with
  | [] -> []
  | x::xs -> toplevel x :: toplevels xs
