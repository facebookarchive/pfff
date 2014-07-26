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

let tok_of_any _any =
  raise Todo

(*****************************************************************************)
(* Conversion *)
(*****************************************************************************)
let rec program xs =
  xs +> List.map toplevel +> List.flatten
  
and toplevel t = 
  match t with
  | Define (name, _body) -> error "use enum not define" (snd name)
  | Undef name -> error "use enum not define" (snd name)
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
    if v.v_storage = Extern
    then []
    else [M.Global (var_decl_global v)]
  | FuncDef def -> [M.FuncDef (func_def def)]


and var_decl_global v =
  match v.v_storage with
  | Extern -> error "extern not supported, use one file" (snd v.v_name)
  | Static -> error "static not supported, use globals" (snd v.v_name)
  | DefaultStorage ->
    (match v.v_init with
    | Some _ -> error "toplevel initializer not supported, use main" 
                   (snd v.v_name)
    | None -> 
      { M.v_name = v.v_name;
        v_type = type_ v.v_type
      }
    )

and type_ x =
  match x with
  | TBase name -> M.TBase name
  | TPointer t -> M.TPointer (type_ t)
  | TArray t -> M.TArray (type_ t)
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
  | None -> error "give a name to parameter" (tok_of_any (Type p.p_type))
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
  | None -> error "unnamed field not supported" (tok_of_any (Type def.fld_type))
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
  | Vars xs -> xs +> List.map var_decl +> List.flatten
  | Asm xs -> error "asm not supported" (tok_of_any (Expr2 (List.hd xs)))
  | _ -> raise Todo

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
      | Some e -> [M.Instr (M.Assign (v.v_name, expr e))]
      | None -> []
      )

and expr _e =
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let (ast_opt, _toks), _stat = Parse_c.parse file in
  match ast_opt with
  | None -> failwith (spf "parsing error on %s" file)
  | Some ast -> program ast

