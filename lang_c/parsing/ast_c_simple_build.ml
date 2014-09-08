(* Yoann Padioleau
 *
 * Copyright (C) 2012, 2014 Facebook
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
 * 
 * We skip the then part of ifdefs.
 * 
 * todo: 
 *  - lift up local union and struct defined in functions?
 *    (hmm but better to rewrite the code I think)
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* for anon struct, which is dangerous! because the main function
 * will return different results given the same input when called
 * two times in a row
 *)
let cnt = ref 0

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

exception ObsoleteConstruct of string * Parse_info.info
exception CplusplusConstruct
exception TodoConstruct of string * Parse_info.info
exception CaseOutsideSwitch
exception MacroInCase

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

let rec ifdef_skipper xs f =

  match xs with
  | [] -> []
  | x::xs ->
    (match f x with
    | None -> x::ifdef_skipper xs f
    | Some ifdef ->
      (match ifdef with
      | Ifdef, tok ->
        pr2_once (spf "skipping: %s" (Parse_info.str_of_info tok));
        (try 
          let (_, x, rest) = 
            xs +> Common2.split_when (fun x -> 
              match f x with
              | Some (IfdefElse, _) -> true
              | Some (IfdefEndif, _) -> true
              | _ -> false
            )
          in
          (match f x with
          | Some (IfdefEndif, _) ->
              ifdef_skipper rest f
          | Some (IfdefElse, _) ->
            let (before, _x, rest) = 
              rest +> Common2.split_when (fun x -> 
                match f x with
                | Some (IfdefEndif, _) -> true
              | _ -> false
              )
            in
            ifdef_skipper before f @ ifdef_skipper rest f
          | _ -> raise Impossible
          )
        with Not_found ->
          failwith (spf "%s: unclosed ifdef" (Parse_info.string_of_info tok))
        )
      | _, tok ->
          failwith (spf "%s: no ifdef" (Parse_info.string_of_info tok))
      )
    )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program xs =
  let env = empty_env () in
  toplevels env xs +> List.flatten

(* ---------------------------------------------------------------------- *)
(* Toplevels *)
(* ---------------------------------------------------------------------- *)

and toplevels env xs =
  ifdef_skipper xs (function IfdefDecl x -> Some x | _ -> None)
    +> List.map (toplevel env)

and toplevel env x =
  match x with
  | DeclElem decl -> declaration env decl
  | CppDirectiveDecl x -> cpp_directive env x

  | (MacroVarTop (_, _)|MacroTop (_, _, _)) ->
      debug (Toplevel x); raise Todo

  | IfdefDecl _ -> raise Impossible (* see ifdef_skipper *)
  (* not much we can do here, at least the parsing statistics should warn the
   * user that some code was not processed
   *)
  | NotParsedCorrectly _ -> []


and declaration env x =
  match x with
  | Func (func_or_else) ->
      (match func_or_else with
      | FunctionOrMethod def ->
          [A.FuncDef (func_def env def)]
      | Constructor _ | Destructor _ ->
          debug (Toplevel (DeclElem x)); raise CplusplusConstruct
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
            | { A.v_type = A.TFunction ft; v_storage = storage; _ } ->
                A.Prototype { A.
                  f_name = x.A.v_name;
                  f_type = ft;
                  f_static = (storage =*= A.Static);
                  f_body = [];
                }
            | _ -> A.Global x
          ))
      | _ -> 
        debug (Toplevel (DeclElem x)); raise Todo
      )

  | EmptyDef _ -> []

  | NameSpaceAnon (_, _)|NameSpaceExtend (_, _)|NameSpace (_, _, _) 
  | ExternCList (_, _, _)|ExternC (_, _, _)|TemplateSpecialization (_, _, _)
  | TemplateDecl _ ->    
      debug (Toplevel (DeclElem x)); raise CplusplusConstruct
  | DeclTodo ->
      debug (Toplevel (DeclElem x)); raise Todo


(* ---------------------------------------------------------------------- *)
(* Functions *)
(* ---------------------------------------------------------------------- *)
and func_def env def = 
  { A.
    f_name = name env def.f_name;
    f_type = function_type env def.f_type;
    f_static = 
      (match def.f_storage with
      | Sto (Static, _) -> true
      | _ -> false
      );
    f_body = compound env def.f_body;
  }

and function_type env x = 
  match x with
  { ft_ret = ret;
    ft_params = params;
    ft_dots = _dotsTODO;
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
    p_register = _regTODO;
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
    v_storage = sto;
  } ->
    (match ni, sto with
    | Some (n, iopt), (NoSto | Sto _)  ->
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
    | Some (n, None), (StoTypedef _) ->
        let def = (name env n, full_type env ft) in
        env.typedefs_toadd <- def :: env.typedefs_toadd;
        None
    | None, NoSto ->
        (match Ast_cpp.unwrap_typeC ft with
        (* it's ok to not have any var decl as long as a type
         * was defined. struct_defs_toadd should not be empty then.
         *)
        | StructDef _ | EnumDef _ -> 
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
     (match xs +> unbrace +> uncomma with
     | [] -> debug (Init x); raise Impossible
     | (InitDesignators ([DesignatorField (_, _)], _, _init))::_ ->
       A.RecordInit (
         xs +> unbrace +> uncomma +> List.map (function
           | InitDesignators ([DesignatorField (_, ident)], _, init) ->
             ident, initialiser env init
           | _ -> debug (Init x); raise Todo
         ))
     | _ ->
       A.ArrayInit ((xs +> unbrace +> uncomma) +> List.map (function
         (* less: todo? *)
         | InitIndexOld ((_, idx, _), ini) ->
             Some (expr env idx), initialiser env ini
         | InitDesignators([DesignatorIndex((_, idx, _))], _, ini) -> 
             Some (expr env idx), initialiser env ini
         | x -> None, initialiser env x
       ))
     )

  (* should be covered by caller *)
  | InitDesignators _ -> debug (Init x); raise Todo
  | InitIndexOld _ | InitFieldOld _ -> debug (Init x); raise Todo

and storage _env x =
  match x with
  | NoSto -> A.DefaultStorage
  | StoTypedef _ -> raise Impossible
  | Sto (y, _) ->
      (match y with
      | Static -> A.Static
      | Extern -> A.Extern
      | Auto | Register -> A.DefaultStorage
      )

(* ---------------------------------------------------------------------- *)
(* Cpp *)
(* ---------------------------------------------------------------------- *)
  
and cpp_directive env x =
  match x with
  | Define (_tok, name, def_kind, def_val) ->
      let v = cpp_def_val x env def_val in
      (match def_kind with
      | DefineVar ->
          [A.Define (name, v)]
      | DefineFunc(args) ->
          [A.Macro(name, 
                 args +> unparen +> uncomma +> List.map (fun (s, ii) ->
                   (s, List.hd ii)
                 ),
                 v)]
      )
  | Include (tok, inc_kind, path) ->
      let s =
        match inc_kind with
        | Local -> "\"" ^ path ^ "\""
        | Standard -> "<" ^ path ^ ">"
        | Weird -> debug (Cpp x); raise Todo
      in
      [A.Include (s, tok)]
  | Undef _ -> debug (Cpp x); raise Todo
  | PragmaAndCo _ -> []

and cpp_def_val for_debug env x = 
  match x with
  | DefineExpr e -> A.CppExpr (expr env e)
  | DefineStmt st -> A.CppStmt (stmt env st)
  | DefineDoWhileZero (st, _) -> A.CppStmt (stmt env st)
  | DefinePrintWrapper (_, (_, e, _), id) -> 
    A.CppExpr (
      A.CondExpr (expr env e,
                  A.Id (name env id),
                  A.Id (name env id)))

  | DefineInit init -> A.CppExpr (initialiser env init)

  | DefineEmpty (* A.CppEmpty*) 
  | ( DefineText _| DefineFunction _
    | DefineType _
    | DefineTodo
    ) -> 
      debug (Cpp for_debug); raise Todo

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
      | Some e -> A.ExprSt (expr env e)
      )
  | DeclStmt block_decl ->
      block_declaration env block_decl

  | Labeled lbl ->
      (match lbl with
      | Label (s, st) ->
          A.Label ((s, List.hd ii), stmt env st)
      | Case _ | CaseRange _ | Default _ ->
          debug (Stmt x); raise CaseOutsideSwitch
      )
  | Jump j ->
      (match j with
      | Goto s -> A.Goto ((s, List.hd ii))
      | Return -> A.Return None;
      | ReturnExpr e -> A.Return (Some (expr env e))
      | Continue -> A.Continue
      | Break -> A.Break
      | GotoComputed _ -> debug (Stmt x); raise Todo
      )

  | Try (_, _, _) ->
      debug (Stmt x); raise CplusplusConstruct

  | (NestedFunc _ | StmtTodo | MacroStmt ) ->
      debug (Stmt x); raise Todo

and compound env (_, xs, _) =
  statements_sequencable env xs +> List.flatten

and statements_sequencable env xs =
  ifdef_skipper xs (function IfdefStmt x -> Some x | _ -> None)
    +> List.map (statement_sequencable env)


and statement_sequencable env x =
  match x with
  | StmtElem st -> [stmt env st]
  | CppDirectiveStmt x -> debug (Cpp x); raise Todo
  | IfdefStmt _ -> raise Impossible

and cases env x =
  let (st, ii) = x in
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
                  | x -> 
                    debug (Stmt (Compound (l, [x], r), ii));
                    raise MacroInCase
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

  | MacroDecl _ -> debug (BlockDecl2 block_decl); raise Todo
      
  | UsingDecl _ | UsingDirective _ | NameSpaceAlias _ -> 
      raise CplusplusConstruct


(* ---------------------------------------------------------------------- *)
(* Expr *)
(* ---------------------------------------------------------------------- *)

and expr env e =
  let (e', toks) = e in
  match e' with
  | C cst -> constant env toks cst

  | Id (n, _) -> A.Id (name env n)

  | RecordAccess (e, n) ->
      A.RecordPtAccess (A.Unary (expr env e, (GetRef,List.hd toks)), name env n)
  | RecordPtAccess (e, n) ->
      A.RecordPtAccess (expr env e, name env n)

  | Cast ((_, ft, _), e) -> 
      A.Cast (full_type env ft, expr env e)

  | ArrayAccess (e1, (_, e2, _)) ->
      A.ArrayAccess (expr env e1, expr env e2)
  | Binary (e1, op, e2) -> A.Binary (expr env e1, (op, List.hd toks), expr env e2)
  | Unary (e, op) -> A.Unary (expr env e, (op, List.hd toks))
  | Infix  (e, op) -> A.Infix (expr env e, (op, List.hd toks))
  | Postfix (e, op) -> A.Postfix (expr env e, (op, List.hd toks))

  | Assignment (e1, op, e2) -> 
      A.Assign ((op, List.hd toks), expr env e1, expr env e2)
  | Sequence (e1, e2) -> 
      A.Sequence (expr env e1, expr env e2)
  | CondExpr (e1, e2opt, e3) ->
      A.CondExpr (expr env e1, 
                 (match e2opt with
                 | Some e2 -> expr env e2
                 | None -> 
                     debug (Expr e); raise Todo
                 ), 
                 expr env e3)
  | Call (e, args) ->
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
    pr2_once "BUG PARSING LOCAL DECL PROBABLY";
    debug (Expr e); 
    raise CplusplusConstruct

  | StatementExpr _
  | ExprTodo
    ->
      debug (Expr e); raise Todo
  | Throw _|DeleteArray (_, _)|Delete (_, _)|New (_, _, _, _, _)
  | CplusplusCast (_, _, _)
  | This _
  | RecordPtStarAccess (_, _)|RecordStarAccess (_, _)
  | TypeId (_, _)
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
  (* TODO! can't just skip it ... *)
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
  | Array ((_, eopt, _), ft) -> 
    A.TArray (Common.map_opt (expr env) eopt, full_type env ft)
  | TypeName (n) -> A.TTypeName (name env n)

  | StructUnionName ((kind, _), name) ->
      A.TStructName (struct_kind env kind, name)
  | StructDef def ->
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
          s_flds = class_members_sequencable env xs +> List.flatten;
        }
        in
        env.struct_defs_toadd <- def' :: env.struct_defs_toadd;
        A.TStructName (struct_kind env kind, name)
      )

  | EnumName (_tok, name) -> A.TEnumName (name)
  | EnumDef (tok, name_opt, xs) ->
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

  | TypeOf (_, _) -> 
    debug (Type x); raise Todo
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


and class_members_sequencable env xs =
  ifdef_skipper xs (function IfdefStruct x -> Some x | _ -> None)
    +> List.map (class_member_sequencable env)

and class_member_sequencable env x =
  match x with
  | ClassElem x -> class_member env x
  | CppDirectiveStruct dir ->
      debug (Cpp dir); raise Todo
  | IfdefStruct _ -> raise Impossible

and fieldkind env x =
  match x with
  | FieldDecl decl ->
      (match decl with
      { v_namei = ni;
        v_type = ft;
        v_storage = sto;
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
  | Class -> raise CplusplusConstruct
