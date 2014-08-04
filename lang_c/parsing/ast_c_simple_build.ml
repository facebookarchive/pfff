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
module A = Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_cpp to Ast_c_simple.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* for anon struct *)
let cnt = ref 0

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception ObsoleteConstruct of string * Ast_cpp.info
exception CplusplusConstruct
exception TodoConstruct of string * Ast_cpp.info

(* not used for now *)
type env = { 
  mutable struct_defs_toadd: A.struct_def list;
  mutable enum_defs_toadd: A.enum_def list;
  mutable typedefs_toadd: A.type_def list;
}

let empty_env () = {
  struct_defs_toadd = [];
  enum_defs_toadd = [];
  typedefs_toadd = [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let debug any =
  let v = Meta_ast_cpp.vof_any any in
  let s = Ocaml.string_of_v v in
  pr2 s

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in

  
  List.map (toplevel env) xs +> List.flatten

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevel env = function
  | CppTop x -> [cpp_directive env x]

  | Func (func_or_else) as x ->
      (match func_or_else with
      | FunctionOrMethod def ->
          [A.FuncDef (func_def env def)]
      | Constructor _ | Destructor _ ->
          debug (Toplevel x); raise CplusplusConstruct
      )

  | BlockDecl bd ->
      (match block_declaration env bd with
      | A.Vars xs -> 
          let structs = env.struct_defs_toadd in
          let enums = env.enum_defs_toadd in
          let typedefs = env.typedefs_toadd in
          env.struct_defs_toadd <- [];
          env.enum_defs_toadd <- [];
          env.typedefs_toadd <- [];
          (structs +> List.map (fun x -> A.StructDef x)) @
          (enums +> List.map (fun x -> A.EnumDef x)) @
          (typedefs +> List.map (fun x -> A.TypeDef x)) @
          (xs +> List.map (fun x ->
            (* could skip extern declaration? *)
            match x with
            | { A.v_type = A.TFunction ft; v_storage = A.DefaultStorage; _ } ->
                A.Prototype { A.
                  f_name = x.A.v_name;
                  f_type = ft;

                  f_body = [];
                }
            | _ -> A.Global x
          ))
      | _ -> raise Todo
      )

  | EmptyDef _ -> []

  | NameSpaceAnon (_, _)|NameSpaceExtend (_, _)|NameSpace (_, _, _) 
  | ExternCList (_, _, _)|ExternC (_, _, _)|TemplateSpecialization (_, _, _)
  | TemplateDecl _
      as x ->
      debug (Toplevel x); raise CplusplusConstruct

  | IfdefTop _ -> 
    pr2_once "SKIPPING ifdefs";
    []

  | (MacroVarTop (_, _)|MacroTop (_, _, _)|DeclTodo) 
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
    ft_dots = _dots; (* todo *)
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
    p_register = _reg;
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
(* Variables *)
(* ---------------------------------------------------------------------- *)
and onedecl env d = 
  match d with
  { v_namei = ni;
    v_type = ft;
    v_storage = ((sto, _inline_or_not), _);
  } ->
    (match ni, sto with
    | Some (n, iopt), ((NoSto | Sto _) as sto)  ->
        let init_opt =
          match iopt with
          | None -> None
          | Some (EqInit (_, ini)) -> Some (initialiser env ini)
          | Some (ObjInit _) -> 
              debug (OneDecl d);
              raise CplusplusConstruct
        in
        Some { A.
          v_name = name env n;
          v_type = full_type env ft;
          v_storage = storage env sto;
          v_init = init_opt;
        }
    | Some (n, None), StoTypedef ->
        let def = (name env n, full_type env ft) in
        env.typedefs_toadd <- def :: env.typedefs_toadd;
        None
    | None, NoSto ->
        (match Ast_cpp.unwrap_typeC ft with
        (* it's ok to not have any var decl as long as a type
         * was defined. struct_defs_toadd should not be empty then.
         *)
        | StructUnion _ | Enum _ -> 
            let _ = full_type env ft in
            None
        (* forward declaration *)
        | StructUnionName _ ->
            None
            
        | _ -> debug (OneDecl d); raise Todo
        )
    | _ -> debug (OneDecl d); raise Todo
    )        

and initialiser env x =
  match x with
  | InitExpr e -> expr env e
  | InitList xs -> 
      A.InitList (List.map (initialiser env) (xs +> unbrace +> uncomma))
  | InitDesignators _ -> raise Todo
  | InitIndexOld _ | InitFieldOld _ -> raise Todo

and storage _env x =
  match x with
  | NoSto -> A.DefaultStorage
  | StoTypedef -> raise Impossible
  | Sto y ->
      (match y with
      | Static -> A.Static
      | Extern -> A.Extern
      | Auto | Register -> A.DefaultStorage
      )

(* ---------------------------------------------------------------------- *)
(* Cpp *)
(* ---------------------------------------------------------------------- *)
  
and cpp_directive env = function
  | Define (_tok, name, def_kind, def_val) as x ->
      let v = cpp_def_val x env def_val in
      (match def_kind with
      | DefineVar ->
          A.Define (name, v)
      | DefineFunc(args) ->
          A.Macro(name, 
                 args +> unparen +> uncomma +> List.map (fun (s, ii) ->
                   (s, List.hd ii)
                 ),
                 v)
      )
  | Include (tok, inc_file) as x ->
      let s =
        (match inc_file with
        | Local xs -> "\"" ^ Common.join "/" xs ^ "\""
        | Standard xs -> "<" ^ Common.join "/" xs ^ ">"
        | Weird _s -> 
            debug (Cpp x); raise Todo
        )
      in
      A.Include (s, tok)
  | Undef (s, tok) -> A.Undef (s, tok)

  | PragmaAndCo _ as x ->
      debug (Cpp x); raise Todo

and cpp_def_val for_debug env x = 
  match x with
  | DefineExpr e -> A.CppExpr (expr env e)
  | DefineStmt st -> A.CppStmt (stmt env st)
  | DefineEmpty -> A.CppEmpty
  | ( DefineText _|DefineInit _|DefineFunction _
    | DefineDoWhileZero _|DefineType _
    | DefineTodo
    ) -> 
      debug (Cpp for_debug);
      raise Todo

(* ---------------------------------------------------------------------- *)
(* Stmt *)
(* ---------------------------------------------------------------------- *)

and stmt env x =
  let (st, ii) = x in
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
          A.For (
            Common2.fmap (expr env) est1,
            Common2.fmap (expr env) est2,
            Common2.fmap (expr env) est3,
            stmt env st
          )

      | MacroIteration _ ->
          debug (Stmt x); raise Todo
      )
  | ExprStatement eopt ->
      (match eopt with
      | None -> A.Block []
      | Some e -> A.Expr (expr env e)
      )
  | DeclStmt block_decl ->
      block_declaration env block_decl

  | Labeled lbl ->
      (match lbl with
      | Label (s, st) ->
          A.Label ((s, List.hd ii), stmt env st)
      | Case _ | CaseRange _ | Default _ ->
          failwith "should be present only in Switch"
      )
  | Jump j ->
      (match j with
      | Goto s -> A.Goto ((s, List.hd ii))
      | Return -> A.Return None;
      | ReturnExpr e -> A.Return (Some (expr env e))
      | Continue -> A.Continue
      | Break -> A.Break
      | GotoComputed _ -> raise Todo
      )

  | Try (_, _, _) ->
      debug (Stmt x); raise CplusplusConstruct

  | (NestedFunc _ | StmtTodo | MacroStmt ) ->
      debug (Stmt x); raise Todo

and compound env (_, x, _) =
  List.map (statement_sequencable env) x +> List.flatten

and statement_sequencable env x =
  match x with
  | StmtElem st -> [stmt env st]
  | CppDirectiveStmt x -> debug (Cpp x); raise Todo
  | IfdefStmt _ -> 
    pr2_once "SKIPPING ifdefs";
    []

and cases env x =
  let (st, _ii) = x in
  match st with
  | Compound (l, xs, r) ->
      let rec aux xs =
        match xs with
        | [] -> []
        | x::xs ->
            (match x with
            | StmtElem ((Labeled (Case (_, st))), _)
            | StmtElem ((Labeled (Default st)), _)
              ->
                let xs', rest =
                  (StmtElem st::xs) +> Common.span (function
                  | StmtElem ((Labeled (Case (_, _st))), _)
                  | StmtElem ((Labeled (Default _st)), _) -> false
                  | _ -> true
                  )
                in
                let stmts = List.map (function
                  | StmtElem st -> stmt env st
                  | _ -> raise Todo
                ) xs' in
                (match x with
                | StmtElem ((Labeled (Case (e, _))), _) ->
                    A.Case (expr env e, stmts)
                | StmtElem ((Labeled (Default _st)), _) ->
                    A.Default (stmts)
                | _ -> raise Impossible
                )::aux rest
            | x -> debug (Body (l, [x], r)); raise Todo
            )
      in
      aux xs
  | _ -> 
      debug (Stmt x); raise Todo

and block_declaration env block_decl =
  match block_decl with
  | DeclList (xs, _) ->
      let xs = uncomma xs in
      A.Vars (Common.map_filter (onedecl env) xs)

  (* todo *)
  | Asm (_tok1, _volatile_opt, _asmbody, _tok2) -> 
      A.Asm []

  | MacroDecl _ -> raise Todo
      
  | UsingDecl _ | UsingDirective _ | NameSpaceAlias _ -> 
      raise CplusplusConstruct


(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

and expr env e =
  let (e', toks) = e in
  match e' with
  | C cst -> constant env toks cst

  | Ident (n, _) -> A.Id (name env n)

  | RecordAccess (e, n) ->
      A.RecordAccess (expr env e, name env n)
  | RecordPtAccess (e, n) ->
      A.RecordAccess (A.Unary (expr env e, DeRef), name env n)

  | Cast ((_, ft, _), e) -> 
      A.Cast (full_type env ft, expr env e)

  | ArrayAccess (e1, (_, e2, _)) ->
      A.ArrayAccess (expr env e1, expr env e2)
  | Binary (e1, op, e2) -> A.Binary (expr env e1, op, expr env e2)
  | Unary (e, op) -> A.Unary (expr env e, op)
  | Infix  (e, op) -> A.Infix (expr env e, op)
  | Postfix (e, op) -> A.Postfix (expr env e, op) 

  | Assignment (e1, op, e2) -> 
      A.Assign (op, expr env e1, expr env e2)
  | Sequence (e1, e2) -> 
      A.Sequence (expr env e1, expr env e2)
  | CondExpr (e1, e2opt, e3) ->
      A.CondExpr (expr env e1, 
                 (match e2opt with
                 | Some e2 -> expr env e2
                 | None -> raise Todo
                 ), 
                 expr env e3)
  | FunCallSimple (n, args) ->
      A.Call (A.Id (name env n),
              Common.map_filter (argument env) (args +> unparen +> uncomma))
  | FunCallExpr (e, args) ->
      A.Call (expr env e,
             Common.map_filter (argument env) (args +> unparen +> uncomma))

  | SizeOfExpr (_tok, e) ->
      A.SizeOf(Left (expr env e))
  | SizeOfType (_tok, (_, ft, _)) ->
      A.SizeOf(Right (full_type env ft))
  | GccConstructor ((_, ft, _), xs) ->
      A.GccConstructor (full_type env ft,
                       initialiser env (InitList xs))

  | ConstructedObject (_, _) ->
    pr2_once "BUG PARSING LOCAL DECL";
    debug (Expr e); 
    raise CplusplusConstruct

  | (TypeIdOfType (_, _)|TypeIdOfExpr (_, _)
  | StatementExpr _
  | ExprTodo
  ) ->
      debug (Expr e); raise Todo
  | Throw _|DeleteArray (_, _)|Delete (_, _)|New (_, _, _, _, _)
  | CplusplusCast (_, _, _)
  | This _
  | RecordPtStarAccess (_, _)|RecordStarAccess (_, _)
      ->
      debug (Expr e); raise CplusplusConstruct

  | ParenExpr (_, e, _) -> expr env e

and constant _env toks x = 
  match x with
  | Int s -> A.Int (s, List.hd toks)
  | Float (s, _) -> A.Float (s, List.hd toks)
  | Char (s, _) -> A.Char (s, List.hd toks)
  | String (s, _) -> A.String (s, List.hd toks)

  | Bool _ -> raise CplusplusConstruct
  | MultiString -> A.String ("TODO", List.hd toks)

and argument env x =
  match x with
  | Left e -> Some (expr env e)
  | Right _w -> 
      pr2 ("type argument, maybe wrong typedef inference!");
      debug (Argument x); 
      None

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
                (* 'char' is a CChar and 'unsigned char' is a Si (_, CChar2) *)
                | CChar2 -> "char"
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

  | FunctionType ft -> A.TFunction (function_type env ft)
  | Array (_less_e, ft) -> A.TArray (full_type env ft)
  | TypeName (n, _) -> A.TTypeName (name env n)

  | StructUnionName ((kind, _), name) ->
      A.TStructName (struct_kind env kind, name)
  | StructUnion def ->
      (match def with
      { c_kind = (kind, tok);
        c_name = name_opt;
        c_inherit = _inh;
        c_members = (_, xs, _);
      } ->
        let name =
          match name_opt with
          | None ->
              incr cnt;
              let s = spf "__anon_struct_%d" !cnt in
              (s, tok)
          | Some n -> name env n
        in
        let def' = { A.
          s_name = name;
          s_kind = struct_kind env kind;
          s_flds = Common2.map (class_member_sequencable env) xs +> List.flatten;
        }
        in
        env.struct_defs_toadd <- def' :: env.struct_defs_toadd;
        A.TStructName (struct_kind env kind, name)
      )

  | EnumName (_tok, name) -> A.TEnumName (name)
  | Enum (tok, name_opt, xs) ->
      let name =
        match name_opt with
        | None ->
            incr cnt;
            let s = spf "__anon_enum_%d" !cnt in
            (s, tok)
        | Some n -> n
      in
      let xs' =
        xs +> unbrace +> uncomma +> List.map (fun eelem ->
          let (name, e_opt) = eelem.e_name, eelem.e_val in
          name, 
          match e_opt with
          | None -> None
          | Some (_tok, e) -> Some (expr env e)
        )
      in
      let def = name, xs' in
      env.enum_defs_toadd <- def :: env.enum_defs_toadd;
      A.TEnumName (name)

  | (TypeOfType (_, _) | TypeOfExpr (_, _))
    -> debug (Type x); raise Todo

  | TypenameKwd (_, _) | Reference _ ->
      debug (Type x); raise CplusplusConstruct

  | ParenType (_, t, _) -> full_type env t

(* ---------------------------------------------------------------------- *)
(* structure *)
(* ---------------------------------------------------------------------- *)
and class_member env x =
  match x with
  | MemberField (fldkind, _) ->
      let xs = uncomma fldkind in
      xs +> List.map (fieldkind env)
  | ( UsingDeclInClass _| TemplateDeclInClass _
    | QualifiedIdInClass (_, _)| MemberDecl _| MemberFunc _| Access (_, _)
    ) ->
      debug (ClassMember x); raise Todo
  | EmptyField _ -> []

and class_member_sequencable env x =
  match x with
  | ClassElem x -> class_member env x
  | CppDirectiveStruct dir ->
      debug (Cpp dir); raise Todo
  | IfdefStruct _ -> raise Todo

and fieldkind env x =
  match x with
  | FieldDecl decl ->
      (match decl with
      { v_namei = ni;
        v_type = ft;
        v_storage = ((sto, _inline_or_not), _);
      } ->
        (match ni, sto with
        | Some (n, None), NoSto ->
            { A.
              fld_name = Some (name env n);
              fld_type = full_type env ft;
            }
        | None, NoSto ->
            { A.
              fld_name = None;
              fld_type = full_type env ft;
            }

        | _ -> debug (OneDecl decl); raise Todo
        )
      )
  | BitField (name_opt, _tok, ft, e) -> 
      let _ = expr env e in
      { A.
        fld_name = name_opt;
        fld_type = full_type env ft;
      }

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

and name _env x =
  match x with
  | (None, [], IdIdent (name)) -> name
  | _ -> debug (Name x); raise CplusplusConstruct

and struct_kind _env = function
  | Struct -> A.Struct
  | Union -> A.Union
  | Class -> raise Todo
