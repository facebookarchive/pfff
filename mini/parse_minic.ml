(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_cpp
open Ast_c
module C = Ast_c
module M = Ast_minic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Just a small wrapper around the C parser (itself a wrapper over the C++
 * parser).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error s tok =
  failwith (spf "%s at %s" s (Parse_info.string_of_info tok))

let error_any s any =
  let v = Meta_ast_c.vof_any any in
  let str = Ocaml.string_of_v v in
  failwith (spf "%s on %s" s str)

(* dupe of graph_code.ml, but avoid deps *)
let cnt = ref 0
let gensym s =
  incr cnt;
  spf "%s__%d" s !cnt

(*****************************************************************************)
(* Conversion *)
(*****************************************************************************)
let rec program xs =
  xs +> List.map toplevel +> List.flatten
  
and toplevel t = 
  match t with
  | Define (name, _body)       -> error "use enum not define" (snd name)
(*  | Undef name                 -> error "use enum not define" (snd name) *)
  | Macro (name, _args, _body) -> error "use function not macros" (snd name)
  (* this is ok, so that gcc can compile the file, but our linking phase
   * do not require it
   *)
  | Include _ -> []
  | Prototype _ -> []

  | StructDef def -> [M.StructDef (struct_def def)]
  | TypeDef (name, _type) -> error "typedefs not supported" (snd name)
  | EnumDef (_name, xs) ->
       xs +> List.map (fun (name, eopt) ->
         (match eopt with
         | Some (Int _) -> ()
         | None -> ()
         | _ -> error "complex expr for enum not supported" (snd name)
         ); 
         M.Constant name
       )

  | Global v -> 
      (* this can be useful for gcc to compile, but our linking phase skip it *)
      if v.v_storage = Extern
      then []
      else [M.Global (var_decl_global v)]
  | FuncDef def -> [M.FuncDef (func_def def)]


and var_decl_global v =
  match v.v_storage with
  | Extern -> error "extern not supported, use one file" (snd v.v_name)
  | Static -> error "static not supported, use regular globals" (snd v.v_name)
  | DefaultStorage ->
    (match v.v_init with
    | Some _ -> 
        error "toplevel initializer not supported, use main" (snd v.v_name)
    | None -> 
      { M.v_name = v.v_name;
        v_type = type_ v.v_type
      }
    )

and type_ x =
  match x with
  | TBase name -> M.TBase name
  | TPointer t -> M.TPointer (type_ t)
  | TArray (_, t) -> M.TArray (type_ t)
  | TFunction (t, params) -> M.TFunction (function_type (t, params))
  | TStructName (kind, name) ->
    (match kind with
    | Struct -> M.TStructName name
    | Union -> error "union not supported" (snd name)
    )
  | TEnumName name -> error "enum type not supported, use int" (snd name)
  | TTypeName name -> error "typedef not supported, inline" (snd name)

and function_type (t, params) =
  (type_ t, params +> List.map parameter)

and parameter p =
  match p.p_name with
  | None -> error_any "give a name to parameter" (Type p.p_type)
  | Some name -> { M.v_name = name; v_type = type_ p.p_type }

and struct_def def =
  match def.s_kind with
  | Union -> error "union not supported" (snd def.s_name)
  | Struct ->
      { M.s_name = def.s_name;
        s_flds = def.s_flds +> List.map field_def
      }

and field_def def = 
  match def.fld_name with
  | None -> error_any "unnamed field not supported" (Type def.fld_type)
  | Some name ->
      { M.v_name = name;
        v_type = type_ def.fld_type;
      }

and func_def def =
  { M.f_name = def.f_name;
    f_type = function_type def.f_type;
    f_body = stmts def.f_body;
  }

and stmts xs = xs +> List.map stmt +> List.flatten

and stmt st =
  match st with
  | ExprSt (Call (Id ("printf", _tok), _xs)) -> []
  (* foo(...) => void local_void_1 = foo(...); *)
  | ExprSt (Call (Id fname, es)) ->
    let name = gensym "local_void", snd fname in
    let typ = M.TBase ("void", snd fname) in
    (M.Local { M.v_name = name; v_type = typ })::
    stmt (ExprSt (Assign ((SimpleAssign, snd name), 
                          Id name, (Call (Id fname, es)))))

  | ExprSt e -> [M.Instr (expr_for_instr e)]
  | Block xs -> stmts xs
  | If (e, st1, st2) -> [M.If (expr_for_var e, stmt st1, stmt st2)]
  | Switch (e, _xs) -> error_any "switch not supported, use if" (Expr e)
  | While (e, st) -> [M.While (expr_for_var e, stmt st)]
  | DoWhile (_, e) -> error_any "dowhile not supported, use while" (Expr e)
  | For (_, _, _, st) -> error_any "for not supported, use while" (Stmt st)
  | Continue | Break -> error "continue or break not supported"
                          (failwith "noTok")
  | Return (Some e) -> [M.Return (expr_for_var e)]
  | Return None -> error "empty return not supported" (failwith "noTok")
  | Label (name, _) | Goto name -> error "label not supported" (snd name)
  | Vars xs -> xs +> List.map var_decl +> List.flatten
  | Asm xs -> error_any "asm not supported" (Expr (List.hd xs))


and var_decl v =
  match v.v_storage with
  | Extern -> error "extern not supported, use one file" (snd v.v_name)
  | Static -> error "static not supported, use globals" (snd v.v_name)
  | DefaultStorage ->
    let decl = 
      M.Local { M.v_name = v.v_name;
                v_type = type_ v.v_type
              }
    in
    decl :: 
      (match v.v_init with
      (* this is ok here *)
      | Some e -> [M.Instr (expr_for_instr 
                              (Assign ((SimpleAssign, snd v.v_name), 
                                       Id v.v_name, e)))]
      | None -> []
      )

and expr_for_instr e = 
  match e with
  | Assign((SimpleAssign, _),
           RecordPtAccess(Id name, fld), Id name2) ->
      M.AssignField (name, fld, name2)
  | Assign((SimpleAssign, _), ArrayAccess(Id name, Id idx), Id name2) ->
      M.AssignArray (name, idx, name2)

  | Assign((SimpleAssign, _), Id name, Unary(Id name2, (GetRef, _))) ->
      M.AssignAddress (name, name2)
  | Assign((SimpleAssign, _), Id name,
           Unary(RecordPtAccess(Id name2, fld), (GetRef, _))) ->
      M.AssignFieldAddress (name, name2, fld)

  | Assign((SimpleAssign, _), Id name, 
           Unary(ArrayAccess(Id name2, Id idx), (GetRef, _)))->
      M.AssignIndexAddress (name, name2, idx)

  | Assign((SimpleAssign, _), Unary(Id name, (DeRef, _)), Id name2) ->
     M.AssignDeref (name, name2)
  | Assign ((SimpleAssign,_), Id name, e) -> 
      M.Assign (name, expr e)
  | _ -> error_any "expected a simple instr, not a C expr" (Expr e)

and expr_for_var e =
  match e with
  | Id name -> name
  | _ -> error_any "expected a var, not a regular expr" (Expr e)

and expr e =
  match e with
  | Int s -> M.Int s
  | String s -> M.String s
  | Float (_, tok) | Char (_, tok) -> error "float/char not supported" tok
  | Id name -> M.Id name

  | Call (Id ("malloc", _), xs) ->
    (match xs with
    | [SizeOf(Right(t))] -> 
        M.Alloc (type_ t)
    | [Binary(Id(var), (Arith(Mul), _), SizeOf(Right(t)))] ->
        M.AllocArray(var, type_ t)
    | _ -> error_any "malloc form not supported" (Expr e)
    )
  | RecordPtAccess(Id name, name2) ->
      M.ObjField (name, name2)
  | ArrayAccess(Id(name1), Id(name2)) ->
      M.ArrayAccess (name1, name2)
  | Unary(Id(name), (DeRef, _)) ->
      M.DeRef name

  | Call (Id ("builtin", tok), xs) -> 
      let name = ("builtin", tok) in
      M.BuiltinCall (name, xs +> List.map expr_for_var)

  | Call (Id name, xs) -> 
      M.StaticCall (name, xs +> List.map expr_for_var)
  | Call(Unary(Id(name), (DeRef, _)), xs) ->
      M.DynamicCall (name, xs +> List.map expr_for_var)

  (* should be handled in caller in expr_for_instr *)
  | Assign _
  (* more general case, not handled *)
  | Call _ | ArrayAccess _ | RecordPtAccess _
  (* advanced features not supported *)
  | Cast _
  | Postfix _ | Infix _ | Unary _ | Binary _ 
  | CondExpr _ | Sequence _
  (* handled only in malloc call context *)
  | SizeOf _
  | ArrayInit _ | RecordInit _ | GccConstructor _
    -> error_any "expected an expr, not a C expr" (Expr e)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let (ast_opt, _toks), _stat = Parse_c.parse file in
  match ast_opt with
  | None -> failwith (spf "parsing error on %s" file)
  | Some ast -> program ast
