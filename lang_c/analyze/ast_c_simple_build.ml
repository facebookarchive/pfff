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

open Ast_cpp
module A = Ast_c_simple

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_cpp to Ast_c_simple.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception ObsoleteConstruct of string * Ast_cpp.info
exception CplusplusConstruct
exception TodoConstruct of string * Ast_cpp.info

(* not used for now *)
type env = unit

let empty_env () = ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec debug any =
  let _ii = Lib_parsing_cpp.ii_of_any any in
  pr2 (Export_ast_cpp.ml_pattern_string_of_any any)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program x =
  let env = empty_env () in
  List.map (toplevel env) x +> List.flatten

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env = function
  | CppTop x -> cpp_directive env x

  | Func (func_or_else) as x ->
      (match func_or_else with
      | FunctionOrMethod def ->
          [A.FuncDef (func_def env def)]
      | Constructor _ | Destructor _ ->
          debug (Toplevel x); raise CplusplusConstruct
      )

  | EmptyDef _ -> []
  | FinalDef _ -> []

  | NameSpaceAnon (_, _)|NameSpaceExtend (_, _)|NameSpace (_, _, _) 
  | ExternCList (_, _, _)|ExternC (_, _, _)|TemplateSpecialization (_, _, _)
  | TemplateDecl _
      as x ->
      debug (Toplevel x); raise CplusplusConstruct
      
  | (MacroVarTop (_, _)|MacroTop (_, _, _)|IfdefTop _|BlockDecl _|DeclTodo) 
      as x ->
      debug (Toplevel x);
      raise Todo

  (* not much we can do here, at least the parsing statistics should warn the
   * user that some code was not processed
   *)
  | NotParsedCorrectly _ -> []

(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_def env def = 
  { A.
    f_name = name env def.f_name;
    f_type = function_type env def.f_type;
    (* f_storage *)
    f_body = compound env def.f_body;
  }

and function_type env x = 
  match x with
  { ft_ret = ret;
    ft_params = params;
    ft_dots = dots; (* todo *)
    ft_const = const;
    ft_throw = throw;
  } ->
    (match const, throw with
    | None, None -> ()
    | _ -> raise CplusplusConstruct
    );
    
    (full_type env ret,
   List.map (parameter env) (params +> unparen +> uncomma)
  )

and parameter env x =
  match x with
  { p_name = n;
    p_type = t;
    p_register = reg;
    p_val = v;
  } ->
    (match v with
    | None -> ()
    | Some _ -> debug (Parameter x); raise CplusplusConstruct
    );
    { A.
      p_name = 
        (match n with
        (* probably a prototype where didn't specify the name *)
        | None -> None
        | Some (name) -> Some name
        );
      p_type = full_type env t;
    }

(* ---------------------------------------------------------------------- *)
(* Cpp *)
(* ---------------------------------------------------------------------- *)
  
and cpp_directive env = function
  | Define (tok, name, def_kind, def_val) as x ->
      (match def_kind, def_val with
      | DefineVar, DefineExpr e ->
          [A.Define (name, expr env e)]
      | _ -> debug (Cpp x); raise Todo
      )
  | Include (tok, inc_file) as x ->
      let s =
        (match inc_file with
        | Local xs -> "\"" ^ Common.join "/" xs ^ "\""
        | Standard xs -> "<" ^ Common.join "/" xs ^ ">"
        | Wierd s -> 
            debug (Cpp x); raise Todo
        )
      in
      [A.Include (s, tok)]
  | (PragmaAndCo _|Undef _) as x ->
      debug (Cpp x); raise Todo

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

and stmt env x =
  let (st, _) = x in
  match st with
  | Compound x -> A.Block (compound env x)
  | Selection s ->
      (match s with
      | If (_, (_, e, _), st1, _, st2) ->
          A.If (expr env e, stmt env st1, stmt env st2)
      | Switch (_, (_, e, _), st) ->
          A.Switch (expr env e, cases env st)
        )
  | Iteration i ->
      (match i with
      | While (_, (_, e, _), st) ->
          A.While (expr env e, stmt env st)
      | DoWhile (_, st, _, (_, e, _), _) ->
          A.DoWhile (stmt env st, expr env e)
      | For (_, (_, ((est1, _), (est2, _), (est3, _)), _), st) ->
          raise Todo
      | MacroIteration _ ->
          raise Todo
      )
  | ExprStatement eopt ->
      (match eopt with
      | None -> A.Block []
      | Some e -> A.Expr (expr env e)
      )

  | (NestedFunc _|Try (_, _, _)|DeclStmt _|Jump _|
Labeled _|StmtTodo|MacroStmt) ->
      debug (Stmt x); raise Todo

and compound env (_, x, _) =
  List.map (statement_sequencable env) x

and statement_sequencable env x =
  match x with
  | StmtElem st -> stmt env st
  | CppDirectiveStmt x -> debug (Cpp x); raise Todo
  | IfdefStmt _ -> raise Todo

and cases env st =
  raise Todo

(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

and expr env e =
  let (e', toks) = e in
  match e' with
  | C cst -> constant env toks cst
  | (TypeIdOfType (_, _)|TypeIdOfExpr (_, _)|GccConstructor (_, _)
  | StatementExpr _|
Cast (_, _)|SizeOfType (_, _)|SizeOfExpr (_, _)|RecordPtStarAccess (_, _)|
RecordStarAccess (_, _)|RecordPtAccess (_, _)|RecordAccess (_, _)|
ArrayAccess (_, _)|Binary (_, _, _)|Unary (_, _)|Infix (_, _)|Postfix (_, _)|
Assignment (_, _, _)|Sequence (_, _)|CondExpr (_, _, _)|FunCallExpr (_, _)|
FunCallSimple (_, _)|Ident (_, _)|ExprTodo) ->
      debug (Expr e); raise Todo

  | Throw _|DeleteArray (_, _)|Delete (_, _)|New (_, _, _, _, _)
  | CplusplusCast (_, _, _)
  | ConstructedObject (_, _) | This _
      ->
      debug (Expr e); raise CplusplusConstruct

  | ParenExpr (_, e, _) -> expr env e

and constant env toks x = 
  match x, toks with
  | Int s, [x] -> A.Int (s,x)
  | (Bool _|Float _|Char _|String _|MultiString), _ -> 
      debug (Constant x); raise Todo
  | _, _ -> 
      debug (Constant x); raise Impossible

(* ---------------------------------------------------------------------- *)
(* Type *)
(* ---------------------------------------------------------------------- *)
and full_type env x =
  let (_qu, (t, ii)) = x in
  match t with
  | Pointer t -> A.TPointer (full_type env t)
  | BaseType t ->
      let s = 
        (match t with
        | Void -> "void"
        | FloatType ft ->
            (match ft with
            | CFloat -> "float" 
            | CDouble -> "double" 
            | CLongDouble -> "long_double"
            )
        | IntType it ->
            (match it with
            | CChar -> "char"
            | Si (si, base) ->
                (match si with
                | Signed -> ""
                | UnSigned -> "unsigned_"
                ) ^
                (match base with
                | CChar2 -> raise Todo
                | CShort -> "short"
                | CInt -> "int"
                | CLong -> "long"
                (* gccext: *)
                | CLongLong -> "long_long" 
                )
            | CBool | WChar_t ->
                debug (Type x); raise CplusplusConstruct
            )
        )
      in
      A.TBase (s, List.hd ii)

  | (TypeOfType (_, _)|TypeOfExpr (_, _)|
TypeName (_, _)|StructUnionName (_, _)|EnumName (_, _)|StructUnion _|
Enum (_, _, _)|FunctionType _|Array (_, _)|Reference _) 
    -> debug (Type x); raise Todo

  | TypenameKwd (_, _) ->
      debug (Type x); raise CplusplusConstruct

  | ParenType (_, t, _) -> full_type env t

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

and name env x =
  match x with
  | (None, [], IdIdent (name)) -> name
  | _ -> debug (Name x); raise CplusplusConstruct
