(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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
open Common2.Infix

open Ast_php
module A = Ast_php_simple

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_php to Ast_php_simple.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* not used for now *)
type env = unit

let empty_env () = ()

exception ObsoleteConstruct of Ast_php.info
exception TodoConstruct of string * Ast_php.info

(* Whether or not we want to store position information in the Ast_simple
 * built here.
 *)
let store_position = ref false

let wrap tok =
  if !store_position then Some tok else None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let opt f env x =
  match x with
  | None -> None
  | Some x -> Some (f env x)

let rec comma_list = function
  | [] -> []
  | Common.Left x  :: rl -> x :: comma_list rl
  | Common.Right _ :: rl -> comma_list rl

let rec comma_list_dots = function
  | [] -> []
  | Common.Left3 x :: rl -> x :: comma_list_dots rl
  | (Common.Middle3 _ | Common.Right3 _) :: rl -> comma_list_dots rl

let brace (_, x, _) = x

let noop = A.Block []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec program top_l =
  let env = empty_env () in
  List.fold_right (toplevel env) top_l []

and toplevel env st acc =
  match st with
  | StmtList stmtl -> List.fold_right (stmt env) stmtl acc
  | FuncDef fd -> A.FuncDef (func_def env fd) :: acc
  | ClassDef cd -> A.ClassDef (class_def env cd) :: acc
  | ConstantDef x -> A.ConstantDef (constant_def env x) :: acc
  | FinalDef _ -> acc
  | NamespaceDef _ -> acc
  (* error recovery is off by default now *)
  | NotParsedCorrectly _ -> raise Common.Impossible

and constant_def env (_, cst_name, _, e, _) =
  { A.cst_name = name env cst_name;
    A.cst_body = expr env e;
  }

and stmt env st acc =
  match st with
  | ExprStmt (e, _) ->
      let e = expr env e in
      A.Expr e :: acc
  (* Why not just acc? because we abuse noop in the abstract interpreter? *)
  | EmptyStmt _ -> noop :: acc
  | Block (_, stdl, _) -> List.fold_right (stmt_and_def env) stdl acc
  | If (_, (_, e, _), st, il, io) ->
      let e = expr env e in
      let st = A.Block (stmt env st []) in
      let il = List.fold_right (if_elseif env) il (if_else env io) in
      A.If (e, st, il) :: acc
  | While (_, (_, e, _), cst) ->
      let cst = colon_stmt env cst in
      A.While (expr env e, cst) :: acc
  | Do (_, st, _, (_, e, _), _) ->
      A.Do (stmt env st [], expr env e) :: acc
  | For (_, _, e1, _, e2, _, e3, _, st) ->
      let st = colon_stmt env st in
      let e1 = for_expr env e1 in
      let e2 = for_expr env e2 in
      let e3 = for_expr env e3 in
      A.For (e1, e2, e3, st) :: acc
  | Switch (_, (_, e, _), scl) ->
      let e = expr env e in
      let scl = switch_case_list env scl in
      A.Switch (e, scl) :: acc
  | Foreach (_, _, e, _, fve, fao, _, cst) ->
      let e = expr env e in
      let fve = foreach_var_either env fve in
      let fao = opt foreach_arrow env fao in
      let cst = colon_stmt env cst in
      A.Foreach (e, fve, fao, cst) :: acc
  | Break (_, e, _) -> A.Break (opt expr env e) :: acc
  | Continue (_, eopt, _) -> A.Continue (opt expr env eopt) :: acc
  | Return (_, eopt, _) -> A.Return (opt expr env eopt) :: acc
  | Throw (_, e, _) -> A.Throw (expr env e) :: acc
  | Try (_, (_, stl, _), c, cl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      let c = catch env c in
      let cl = List.map (catch env) cl in
      A.Try (stl, c, cl) :: acc
  | Echo (tok, el, _) ->
      A.Expr (A.Call (A.Id (A.builtin "echo", wrap tok),
                     (List.map (expr env) (comma_list el)))) :: acc
  | Globals (_, gvl, _) ->
      A.Global (List.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (_, svl, _) ->
      A.StaticVars (List.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, tok) ->
      A.Expr (A.Call (A.Id (A.builtin "echo", wrap tok),
                     [A.String (s, wrap tok)])) :: acc
  | Use (tok, fn, _) ->
      raise (TodoConstruct ("use", tok))
  | Unset (tok, (_, lp, _), e) ->
      let lp = comma_list lp in
      let lp = List.map (lvalue env) lp in
      A.Expr (A.Call (A.Id (A.builtin "unset", wrap tok), lp)) :: acc
  (* http://php.net/manual/en/control-structures.declare.php *)
  | Declare (tok, args, stmt) ->
      (match args, stmt with
      (* declare(strict=1); (or 0) can be skipped,
       * See 'i wiki/index.php/Pfff/Declare_strict' *)
      | (_,[Common.Left((Name(("strict",_)),(_,Sc(C(Int((("1"|"0"),_)))))))],_),
      SingleStmt(EmptyStmt(_))
      (* declare(ticks=1); can be skipped too.
       * http://www.php.net/manual/en/control-structures.declare.php#control-structures.declare.ticks
       *)
      | (_,[Common.Left((Name(("ticks",_)), (_,Sc(C(Int((("1"),_)))))))],_),
      SingleStmt(EmptyStmt(_))
      ->
        acc
      |  _ -> raise (TodoConstruct ("Declare", tok))
      )

  | FuncDefNested fd -> A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd -> A.ClassDef (class_def env cd) :: acc

   (* this is not yet used in our codebase *)
  | TypedDeclaration _ -> raise Common.Impossible
  | IfColon (tok, _, _, _, _, _, _, _) -> raise (ObsoleteConstruct tok)

and if_elseif env (_, (_, e, _), st) acc =
  let e = expr env e in
  let st = match stmt env st [] with [x] -> x | l -> A.Block l in
  A.If (e, st, acc)

and if_else env = function
  | None -> noop
  | Some (_, (If _ as st)) ->
      (match stmt env st [] with
      | [x] -> x
      | l -> assert false)
  | Some (_, st) -> A.Block (stmt env st [])

and stmt_and_def env st acc = stmt env st acc

and expr env = function
  | Sc sc -> scalar env sc
  | Lv lv -> lvalue env lv
  | Binary (e1, (bop, _), e2) ->
      let e1 = expr env e1 in
      let e2 = expr env e2 in
      A.Binop (bop, e1, e2)
  | Unary ((uop, _), e) ->
      let e = expr env e in
      A.Unop (uop, e)
  | Assign (e1, _, e2) -> A.Assign (None, lvalue env e1, expr env e2)
  | AssignOp (lv, (op, _), e) ->
      let op = assignOp env op in
      A.Assign (Some op, lvalue env lv, expr env e)
  | Postfix (v, (fop, _)) -> A.Postfix (fop, lvalue env v)
  | Infix ((fop, _), v) -> A.Infix (fop, lvalue env v)
  | CondExpr (e1, _, None, _, e3) ->
      let e = expr env e1 in
      A.CondExpr (e, e, expr env e3);
  | CondExpr (e1, _, Some e2, _, e3) ->
      A.CondExpr (expr env e1, expr env e2, expr env e3)
  | AssignList (_, (_, la, _), _, e) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      let e = expr env e in
      A.Assign (None, A.List la, e)
  | ArrayLong (_, (tok, apl, _))
  | ArrayShort (tok, apl, _) ->
      let apl = comma_list apl in
      let apl = List.map (array_pair env) apl in
      A.ConsArray (None, apl)
  | VectorLit (_, (_, vel, _)) ->
      let vel = comma_list vel in
      let vel = List.map (vector_elt env) vel in
      A.ConsVector vel
  | MapLit (t, (_, mel, _)) ->
      let kind = match Parse_info.str_of_info t with
        | "Map"       -> A.Map
        | "StableMap" -> A.StableMap
        | _ -> raise Common.Impossible
      in
      let mel = comma_list mel in
      let mel = List.map (map_elt env) mel in
      A.ConsMap (kind, mel)
  | New (_, cn, args) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (cn, args)
  | Clone (tok, e) ->
      A.Call (A.Id (A.builtin "clone", wrap tok), [expr env e])
  | AssignRef (e1, _, _, e2) ->
      let e1 = lvalue env e1 in
      let e2 = lvalue env e2 in
      A.Assign (None, e1, A.Ref e2)
  (* this is almost never used in our codebase, just in some third party code *)
  | AssignNew (e1, _, _, new_tok, class_ref, args) ->
      let e1 = lvalue env e1 in
      let e2 = expr env (New (new_tok, class_ref, args)) in
      A.Assign (None, e1, A.Ref e2)
  | Cast ((c, _), e) ->
      A.Cast (c, expr env e)
  | CastUnset (tok, _) ->
      raise (TodoConstruct ("expr CastUnset", tok))
  | InstanceOf (e, _, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (e, cn)
  | Eval (tok, (_, e, _)) ->
      A.Call (A.Id (A.builtin "eval", wrap tok), [expr env e])
  | Lambda ld ->
      A.Lambda (lambda_def env ld)
  | Exit (tok, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) -> []
        | Some (_, Some e, _) -> [expr env e]
      in
      A.Call (A.Id (A.builtin "exit", wrap tok), arg)
  | At (tok, e) ->
      let arg = expr env e in
      A.Call (A.Id (A.builtin "at", wrap tok), [arg])
  | Print (tok, e) ->
      A.Call (A.Id (A.builtin "print", wrap tok), [expr env e])
  | BackQuote (tok, el, _) ->
      A.Call (A.Id (A.builtin "exec", wrap tok (* not really an exec token *)),
             [A.Guil (List.map (encaps env) el)])
  | Include (tok, e) ->
      A.Call (A.Id (A.builtin "include", wrap tok), [expr env e])
  | IncludeOnce (tok, e) ->
      A.Call (A.Id (A.builtin "include_once", wrap tok), [expr env e])
  | Require (tok, e) ->
      A.Call (A.Id (A.builtin "require", wrap tok), [expr env e])
  | RequireOnce (tok, e) ->
      A.Call (A.Id (A.builtin "require_once", wrap tok), [expr env e])

  | Empty (tok, (_, lv, _)) ->
      A.Call (A.Id (A.builtin "empty", wrap tok), [lvalue env lv])
  | Isset (tok, (_, lvl, _)) ->
      A.Call (A.Id (A.builtin "isset", wrap tok),
             List.map (lvalue env) (comma_list lvl))
  | XhpHtml xhp -> A.Xhp (xhp_html env xhp)

  | Yield (tok, e) ->
      A.Call (A.Id (A.builtin "yield", wrap tok), [expr env e])
  (* todo? merge in one yield_break? *)
  | YieldBreak (tok, tok2) ->
      A.Call (A.Id (A.builtin "yield", wrap tok),
             [A.Id (A.builtin "yield_break", wrap tok2)])
  | SgrepExprDots _ ->
      (* should never use the abstract interpreter on a sgrep pattern *)
      raise Common.Impossible
  | ParenExpr (_, e, _) -> expr env e

and lambda_def env (l_use, ld) =
  let _, params, _ = ld.f_params in
  let params = comma_list_dots params in
  let _, body, _ = ld.f_body in
  { A.f_ref = ld.f_ref <> None;
    A.f_name = (A.special "_lambda", wrap ld.f_tok);
    A.f_params = List.map (parameter env) params;
    A.f_return_type = None;
    A.f_body = List.fold_right (stmt_and_def env) body [];
    A.f_kind = A.AnonLambda;
    A.m_modifiers = [];
    A.f_attrs = attributes env ld.f_attrs;
    A.l_uses =
      (match l_use with
      | None -> []
      | Some (_, (_lp, xs, _rp)) ->
          comma_list xs +> List.map (function
          | LexicalVar (is_ref, name) -> is_ref <> None, dname name
          )
      );

  }

and scalar env = function
  | C cst -> constant env cst
  | ClassConstant (q, s) ->
      A.Class_get (A.Id (qualifier env q), A.Id (name env s))
  | Guil (_, el, _) -> A.Guil (List.map (encaps env) el)
  | HereDoc (_, el, _) -> A.Guil (List.map (encaps env) el)

and constant env = function
  | Int (n, _) -> A.Int n
  | Double (n, _) -> A.Double n
  | String (s, tok) -> A.String (s, wrap tok)
  | CName n -> A.Id (name env n)
  | PreProcess (cpp, tok) -> cpp_directive env tok cpp
  (* no reason to use the abstract interpreter on xdebug traces *)
  | XdebugClass _ | XdebugResource -> raise Common.Impossible

and cpp_directive env tok = function
  | Line      -> A.Id (A.builtin "__LINE__", wrap tok)
  | File      -> A.Id (A.builtin "__FILE__", wrap tok)
  | ClassC    -> A.Id (A.builtin "__CLASS__", wrap tok)
  | MethodC   -> A.Id (A.builtin "__METHOD__", wrap tok)
  | FunctionC -> A.Id (A.builtin "__FUNCTION__", wrap tok)
  | Dir       -> A.Id (A.builtin "__DIR__", wrap tok)
  | TraitC    -> A.Id (A.builtin "__TRAIT__", wrap tok)

and name env = function
  | Name (s, tok) -> s, wrap tok
  | XhpName (tl, tok) ->
      A.string_of_xhp_tag tl, wrap tok

and dname = function
  | DName (s, tok) ->
      if s.[0] = '$'
      then failwith "dname: the string has a dollar, weird";
      (* We abuse Id to represent both variables and functions/classes
       * identifiers in ast_php_simple, so to avoid collision
       * we prepend a $ (the $ was removed in ast_php.ml and parse_php.ml)
       *)
      ("$"^s, wrap tok)

and hint_type env = function
  | Hint q -> A.Hint (class_name_or_selfparent env q)
  | HintArray _ -> A.HintArray
  | HintQuestion (i, t) -> A.HintQuestion (hint_type env t)
  | HintTuple v1 -> A.HintTuple (List.map (hint_type env) (comma_list (brace v1)))
  | HintCallback (_, (_, args, ret), _) ->
      let args = List.map (hint_type env) (comma_list_dots (brace args)) in
      let ret  = Common2.fmap (hint_type env) ret in
      A.HintCallback (args, ret)


and qualifier env (cn, _) = class_name_or_selfparent env cn

and class_name_or_selfparent env = function
   | ClassName (fqcn, _) -> name env fqcn (* TODO: support type args? *)
   | Self tok -> (A.special "self", wrap tok)
   | Parent tok -> (A.special "parent", wrap tok)
   | LateStatic tok -> (A.special "static", wrap tok)

and class_name_reference env = function
   | ClassNameRefStatic cn -> A.Id (class_name_or_selfparent env cn)
   | ClassNameRefDynamic (lv, []) -> lvalue env lv
   | ClassNameRefDynamic (lv, [tok, obj_prop]) ->
       let obj = lvalue env lv in
       obj_property env tok obj obj_prop
   | ClassNameRefDynamic (lv, (tok, _)::xs) ->
       raise (TodoConstruct ("ClassNameRefDynamic", tok))

and lvalue env = function
  | Var (dn, scope) -> A.Id (dname dn)
  | This tok -> A.This ("$this", wrap tok)
  | NewLv (_, (_, cn, args), _) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (cn, args)
  | VArrayAccess (lv, (tok, e, _)) ->
      let lv = lvalue env lv in
      let e = opt expr env e in
      A.Array_get (lv, e)
  (* one can use $o[xxx] or $o{xxx} apparently *)
  | VBraceAccess (lv, (tok, e, _)) ->
      A.Array_get (lvalue env lv, Some (expr env e))
  | VArrayAccessXhp (e1, (tok, e2, _)) ->
      let e1 = expr env e1 in
      let e2 = opt expr env e2 in
      A.Array_get (e1, e2)
  | VBrace (tok, (_, e, _)) ->
      A.Call (A.Id ((A.builtin "eval_var", wrap tok)), [expr env e])
  | Indirect (e, (Dollar tok)) ->
      A.Call (A.Id (A.builtin "eval_var", wrap tok), [lvalue env e])
  | VQualifier (q, v)  ->
      let (_, tok) = q in
      A.Class_get (A.Id (qualifier env q),
                  A.Call (A.Id (A.builtin "eval_var", wrap tok),[lvalue env v]))
  | ClassVar (q, dn) ->
      A.Class_get (A.Id (qualifier env q), A.Id (dname dn))
  | FunCallSimple (f, (tok, args, _)) ->
      let f = name env f in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (A.Id f, args)
  | FunCallVar (q, lv, (tok, argl, _)) ->
      let argl = comma_list argl in
      let argl = List.map (argument env) argl in
      let lv = lvalue env lv in
      let lv = match q with
        | None -> lv
        | Some q -> A.Class_get (A.Id (qualifier env q), lv)
      in
      A.Call (lv, argl)
  | StaticMethodCallSimple (q, n, (tok, args, _)) ->
      let f = A.Class_get (A.Id (qualifier env q), A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | MethodCallSimple (e, _, n, (tok, args, _)) ->
      let f = lvalue env e in
      let f = A.Obj_get (f, A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | StaticMethodCallVar (lv, _, n, (tok, args, _)) ->
      let f = A.Class_get (lvalue env lv, A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | StaticObjCallVar (lv, tok, lv2, args) ->
      raise (TodoConstruct ("StaticObjCallVar", tok))

  | ObjAccessSimple (lv, _, n) -> A.Obj_get (lvalue env lv, A.Id (name env n))
  | ObjAccess (lv, oa) ->
      let lv = lvalue env lv in
      obj_access env lv oa
  | DynamicClassVar (lv, _, lv2) ->
      A.Class_get (lvalue env lv, lvalue env lv2)


and obj_access env obj (tok, objp, args) =
  let e = obj_property env tok obj objp in
  match args with
  | None -> e
  | Some (tok, args, _) ->
      let args = comma_list args in
      let args = List.map (argument env) args in
      (* TODO CHECK THIS *)
      A.Call (e, args)

and obj_property env tok obj = function
  | ObjProp objd -> obj_dim env obj objd
  | ObjPropVar lv ->
      A.Obj_get (obj, A.Call (A.Id (A.builtin "eval_var_field", wrap tok),
                             [lvalue env lv]))

and obj_dim env obj = function
  | OName n -> A.Obj_get (obj, A.Id(name env n))
  | OBrace (_, e, _) ->
      A.Obj_get (obj, expr env e)
  (* again, one can use the [] or {} syntax *)
  | OArrayAccess (x, (tok, e, _)) ->
      let e = opt expr env e in
      let x = obj_dim env obj x in
      A.Array_get (x, e)
  (* this is almost never used in our codebase, just in some third-party code.*)
  | OBraceAccess (x, (tok, e, _)) ->
      let e = expr env e in
      let x = obj_dim env obj x in
      A.Array_get(x, Some e)

and argument env = function
  | Arg e -> expr env e
  | ArgRef (_, e) -> A.Ref (lvalue env e)

(* todo: xhp class declaration ?*)
and class_def env c =
  let _, body, _ = c.c_body in
  {
    A.c_kind = class_type env c.c_type ;
    A.c_name = name env c.c_name;
    A.c_attrs = attributes env c.c_attrs;
    A.c_extends =
      (match c.c_extends with
      | None -> None
      | Some (_, x) -> Some (name env x)
      );
    A.c_uses =
      List.fold_right (class_traits env) body [];
    A.c_implements =
      (match c.c_implements with
      | None -> []
      | Some x -> interfaces env x
      );
    A.c_constants =
      List.fold_right (class_constants env) body [];
    A.c_variables =
      List.fold_right (class_variables env) body [];
    A.c_methods =
      List.fold_right (class_body env) body [];
  }

and class_type env = function
  | ClassRegular _ -> A.ClassRegular
  | ClassFinal _ -> A.ClassFinal
  | ClassAbstract _ -> A.ClassAbstract
  | Interface _ -> A.Interface
  | Trait _ -> A.Trait

and interfaces env (_, intfs) =
  let intfs = comma_list intfs in
  List.map (fun x -> (name env x)) intfs

and class_traits env x acc =
  match x with
  | UseTrait (_, l, _) ->
      List.map (name env) (comma_list l) @ acc
  | _ -> acc

and class_constants env st acc =
  match st with
  | ClassConstants (_, cl, _) ->
      List.fold_right (
      fun (n, ss) acc ->
        ({A.cst_name = name env n; cst_body= static_scalar_affect env ss})::acc
     ) (comma_list cl) acc
  | _ -> acc

and static_scalar_affect env (_, ss) = static_scalar env ss
and static_scalar env a = expr env a

and class_variables env st acc =
  match st with
  | ClassVariables (m, ht, cvl, _) ->
      let cvl = comma_list cvl in
      let m =
        match m with
        | NoModifiers _ -> []
        | VModifiers l -> List.map (fun (x, _) -> x) l
      in
      let ht = opt hint_type env ht in
      List.map (fun (n, ss) ->
          let name = dname n in
          let value = opt static_scalar_affect env ss in
          {
            A.cv_name = name;
            A.cv_value = value;
            A.cv_modifiers = m;
            A.cv_type = ht;
          }
       ) cvl @ acc
  | _ -> acc


and class_body env st acc =
  match st with
  | Method md ->
      method_def env md :: acc
  (* TODO? or we don't care and it's ok? *)
  | XhpDecl _ ->
      acc
  | (ClassVariables (_, _, _, _)|ClassConstants (_, _, _)|UseTrait _) -> acc

and method_def env m =
  let _, params, _ = m.f_params in
  let params = comma_list_dots params in
  let mds = List.map (fun (x, _) -> x) m.f_modifiers in
  { A.m_modifiers = mds;
    A.f_ref = (match m.f_ref with None -> false | Some _ -> true);
    A.f_name = name env m.f_name;
    A.f_attrs = attributes env m.f_attrs;
    A.f_params = List.map (parameter env) params ;
    A.f_return_type = opt hint_type env m.f_return_type;
    A.f_body = method_body env m.f_body;
    A.f_kind = A.Method;
    A.l_uses = [];
  }

and method_body env (_, stl, _) =
  List.fold_right (stmt_and_def env) stl []

and parameter env
 { p_type = t; p_ref = r; p_name = name; p_default = d; p_attrs = a} =
  { A.p_type = opt hint_type env t;
    A.p_ref = r <> None;
    A.p_name = dname name;
    A.p_default = opt static_scalar_affect env d;
    A.p_attrs = attributes env a;
  }

and func_def env f =
  let _, params, _ = f.f_params in
  let params = comma_list_dots params in
  let _, body, _ = f.f_body in
  { A.f_ref = f.f_ref <> None;
    A.f_name = name env f.f_name;
    A.f_attrs = attributes env f.f_attrs;
    A.f_params = List.map (parameter env) params;
    A.f_return_type = opt hint_type env f.f_return_type;
    A.f_body = List.fold_right (stmt_and_def env) body [];
    A.f_kind = A.Function;
    A.m_modifiers = [];
    A.l_uses = [];
  }

and xhp_html env = function
  | Xhp (tag, attrl, _, body, _) ->
      let tag, tok = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = (A.string_of_xhp_tag tag, wrap tok);
        A.xml_attrs = attrl;
        A.xml_body = List.map (xhp_body env) body;
      }
  | XhpSingleton (tag, attrl, _) ->
      let tag, tok = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = (A.string_of_xhp_tag tag, wrap tok);
        A.xml_attrs = attrl;
        A.xml_body = [];
      }

and xhp_attribute env ((n, tok), _, v) =
  (n, wrap tok), xhp_attr_value env v

and xhp_attr_value env = function
  | XhpAttrString (_, l, _) ->
      A.Guil (List.map (encaps env) l)
  | XhpAttrExpr (_, e, _) ->
      (expr env e)
  | SgrepXhpAttrValueMvar _ ->
      (* should never use the abstract interpreter on a sgrep pattern *)
      raise Common.Impossible

and xhp_body env = function
  | XhpText (s, _) -> A.XhpText s
  | XhpExpr (_, e, _) -> A.XhpExpr (expr env e)
  | XhpNested xml -> A.XhpXml (xhp_html env xml)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

and encaps env = function
  | EncapsString (s, tok) -> A.String (s, wrap tok)
  | EncapsVar v -> lvalue env v
  | EncapsCurly (_, lv, _) ->  lvalue env lv
  | EncapsDollarCurly (_, lv, _) -> lvalue env lv
  | EncapsExpr (_, e, _) -> expr env e

and array_pair env = function
  | ArrayExpr e -> A.Aval (expr env e)
  | ArrayRef (_, lv) -> A.Aval (A.Ref (lvalue env lv))
  | ArrayArrowExpr (e1, _, e2) -> A.Akval (expr env e1, expr env e2)
  | ArrayArrowRef (e1, _, _, lv) -> A.Akval (expr env e1, A.Ref (lvalue env lv))

and vector_elt env = function
  | VectorExpr e -> expr env e
  | VectorRef (_, e) -> A.Ref (lvalue env e)

and map_elt env = function
  | MapArrowExpr (e1, _, e2) -> (expr env e1, expr env e2)
  | MapArrowRef (e1, _, _, lv) -> (expr env e1, A.Ref (lvalue env lv))

and for_expr env el = List.map (expr env) (comma_list el)

and colon_stmt env = function
  | SingleStmt st -> stmt env st []
  | ColonStmt (_, stl, _, _) -> List.fold_right (stmt_and_def env) stl []
(*of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *) *)

and switch_case_list env = function
  | CaseList (_, _, cl, _) -> List.map (case env) cl
  | CaseColonList (tok, _, _, _, _) -> raise (ObsoleteConstruct tok)

and case env = function
  | Case (_, e, _, stl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Case (expr env e, stl)
  | Default (_, _, stl) ->
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Default stl

and foreach_arrow env (_, fv) = foreach_variable env fv
and foreach_variable env (r, lv) =
  let e = lvalue env lv in
  let e = if r <> None then A.Ref e else e in
  e

and foreach_var_either env = function
  | Common.Left fv -> foreach_variable env fv
  | Common.Right lv -> lvalue env lv

and catch env (_, (_, (fq, dn), _), (_, stdl, _)) =
  let stdl = List.fold_right (stmt_and_def env) stdl [] in
  let fq = name env fq in
  let dn = dname dn in
  A.Hint fq, dn, stdl

and static_var env (x, e) =
  dname x, opt static_scalar_affect env e

and list_assign env x acc =
  match x with
  | ListVar lv -> (lvalue env lv) :: acc
  | ListList (_, (_, la, _)) ->
      let la = comma_list la in
      let la = List.fold_right (list_assign env) la [] in
      A.List la :: acc
  | ListEmpty -> acc

and assignOp env = function
  | AssignOpArith aop -> Arith aop
  | AssignConcat -> BinaryConcat

and global_var env = function
  | GlobalVar dn -> A.Id (dname dn)
  (* this is used only once in our codebase, and it should not ... *)
  | GlobalDollar (tok, lv) ->
      A.Call (A.Id ((A.builtin "eval_var", wrap tok)), [lvalue env lv])
  | GlobalDollarExpr (tok, _) ->
      raise (TodoConstruct ("GlobalDollarExpr", tok))

and attributes env = function
  | None -> []
  | Some (_, xs, _) ->
    let xs = comma_list xs in
    xs +> List.map (function
    | Attribute (s, tok) -> A.Id (s, wrap tok)
    | AttributeWithArgs ((s, tok), (_, xs, _)) ->
      A.Call (A.Id (s, wrap tok), List.map (static_scalar env) (comma_list xs))
    )

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)
let program_with_position_information prog =
  Common.save_excursion store_position true (fun () ->
    program prog
  )

(*****************************************************************************)
(* For cmf *)
(*****************************************************************************)
let func_def x = func_def (empty_env()) x
let class_def x = class_def (empty_env()) x
let constant_def x = constant_def (empty_env()) x
