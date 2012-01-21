(* Julien Verlaguet
 *
 * Copyright (C) 2011 Facebook
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
open Ast_php

module Int = struct type t = int let compare = (-) end
module ISet = Set.Make(Int)
module IMap = Map.Make(Int)

module A = Ast_php_simple
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Ast_php to Ast_php_simple
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* not used for now *)
type env = unit

exception ObsoleteConstruct

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

let empty_env () = ()

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
  (* error recovery is off by default now *)
  | NotParsedCorrectly _ -> raise Common.Impossible

and constant_def env (_, cst_name, _, e, _) =
  {
    A.cst_name = name env cst_name;
    A.cst_body = expr env e;
  }

and stmt env st acc =
  match st with
  | ExprStmt (e, _) ->
      let e = expr env e in
      A.Expr e :: acc
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
      A.Expr (A.Call (A.Id (A.builtin "echo", tok),
                     (List.map (expr env) (comma_list el)))) :: acc
  | Globals (_, gvl, _) -> A.Global (List.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (_, svl, _) ->
      A.StaticVars (List.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, tok) ->
      A.Expr (A.Call (A.Id (A.builtin "echo", tok),
                     [A.String s])) :: acc
  | Use (tok, fn, _) ->
      A.Expr (A.Call (A.Id (A.builtin "use", tok),
                     [A.String (use_filename env fn)])) :: acc
  | Unset (tok, (_, lp, _), e) ->
      let lp = comma_list lp in
      let lp = List.map (lvalue env) lp in
      A.Expr (A.Call (A.Id (A.builtin "unset", tok), lp)) :: acc
  | Declare _ ->
      failwith "Declare"
  | TypedDeclaration _ ->
      (* this is not yet used in our codebase *)
      raise Common.Impossible
  | IfColon _ ->
      raise ObsoleteConstruct


and use_filename env = function
  | UseDirect (s, _) -> s
  | UseParen (_, (s, _), _) -> s

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

and stmt_and_def env st acc =
  match st with
  | Stmt st -> stmt env st acc
  | FuncDefNested fd -> A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd -> A.ClassDef (class_def env cd) :: acc

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
  | ArrayLong (_, (_, apl, _)) | ArrayShort (_, apl, _) ->
      let apl = comma_list apl in
      let apl = List.map (array_pair env) apl in
      A.ConsArray apl
  | New (_, cn, args) ->
      let args =
        match args with
        | None -> []
        | Some (_, cl, _) -> List.map (argument env) (comma_list cl)
      in
      let cn = class_name_reference env cn in
      A.New (cn, args)
  | Clone (tok, e) ->
      A.Call (A.Id (A.builtin "clone", tok), [expr env e])
  | AssignRef (e1, _, _, e2) ->
      let e1 = lvalue env e1 in
      let e2 = lvalue env e2 in
      A.Assign (None, e1, A.Ref e2)
  | AssignNew _ ->
      failwith "expr AssignNew"
  | Cast ((c, _), e) ->
      A.Cast (c, expr env e)
  | CastUnset _ ->
      failwith "expr CastUnset"
  | InstanceOf (e, _, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (e, cn)
  | Eval (tok, (_, e, _)) ->
      A.Call (A.Id (A.builtin "eval", tok), [expr env e])
  | Lambda ld ->
      A.Lambda (lambda_def env ld)
  | Exit (tok, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) -> []
        | Some (_, Some e, _) -> [expr env e]
      in
      A.Call (A.Id (A.builtin "exit", tok), arg)
  | At (tok, e) ->
      A.Id (A.builtin "@", tok)
  | Print (tok, e) ->
      A.Call (A.Id (A.builtin "print", tok), [expr env e])
  | BackQuote (tok, el, _) ->
      A.Call (A.Id (A.builtin "exec", tok (* not really an exec token *)),
             [A.Guil (List.map (encaps env) el)])
  | Include (tok, e) ->
      A.Call (A.Id (A.builtin "include", tok), [expr env e])
  | IncludeOnce (tok, e) ->
      A.Call (A.Id (A.builtin "include_once", tok), [expr env e])
  | Require (tok, e) ->
      A.Call (A.Id (A.builtin "require", tok), [expr env e])
  | RequireOnce (tok, e) ->
      A.Call (A.Id (A.builtin "require_once", tok), [expr env e])
  | Empty (tok, (_, lv, _)) ->
      A.Call (A.Id (A.builtin "empty", tok), [lvalue env lv])
  | Isset (tok, (_, lvl, _)) ->
      A.Call (A.Id (A.builtin "isset", tok),
             List.map (lvalue env) (comma_list lvl))
  | XhpHtml xhp -> A.Xhp (xhp_html env xhp)
  | Yield (tok, e) ->
      A.Call (A.Id (A.builtin "yield", tok), [expr env e])
  | YieldBreak (tok, tok2) ->
      A.Call (A.Id (A.builtin "yield", tok),
             [A.Id (A.builtin "yield_break", tok2)])
  | SgrepExprDots _ ->
      (* should never use the abstract interpreter on a sgrep pattern *)
      raise Common.Impossible
  | ParenExpr (_, e, _) -> expr env e

(* TODO: uses ??? *)
and lambda_def env ld =
  let _, params, _ = ld.l_params in
  let params = comma_list_dots params in
  let _, body, _ = ld.l_body in
  { A.f_ref = ld.l_ref <> None;
    A.f_name = (A.special "_lambda", Ast_php.fakeInfo "_lambda");
    A.f_params = List.map (parameter env) params;
    A.f_return_type = None;
    A.f_body = List.fold_right (stmt_and_def env) body [];
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
  | String (s, _) -> A.String s
  | CName n -> A.Id (name env n)
  | PreProcess (cpp, tok) -> cpp_directive env tok cpp
  (* no reason to use the abstract interpreter on xdebug traces *)
  | XdebugClass _ | XdebugResource -> raise Common.Impossible

and cpp_directive env tok = function
  | Line      -> A.Id (A.builtin "__LINE__", tok)
  | File      -> A.Id (A.builtin "__FILE__", tok)
  | ClassC    -> A.Id (A.builtin "__CLASS__", tok)
  | MethodC   -> A.Id (A.builtin "__METHOD__", tok)
  | FunctionC -> A.Id (A.builtin "__FUNCTION__", tok)
  | Dir       -> A.Id (A.builtin "__DIR__", tok)
  | TraitC    -> A.Id (A.builtin "__TRAIT__", tok)

and name env = function
  | Name (s, tok) -> s, tok
  | XhpName (tl, tok) ->
      A.string_of_xhp_tag tl, tok

and dname = function
  | DName (s, tok) ->
      if s.[0] = '$'
      then failwith "dname: the string has a dollar, weird";
      (* We abuse Id to represent both variables and functions/classes
       * identifiers in ast_ph_simple, so to avoid collision
       * we prepend a $ (the $ was removed in ast_php.ml and parse_php.ml)
       *)
      ("$"^s, tok)

and hint_type env = function
  | Hint q -> A.Hint (fst (class_name_or_selfparent env q))
  | HintArray _ -> A.HintArray

and qualifier env (cn, _) = class_name_or_selfparent env cn

and class_name_or_selfparent env = function
   | ClassName fqcn -> name env fqcn
   | Self tok -> (A.special "self", tok)
   | Parent tok -> (A.special "parent", tok)
   | LateStatic tok -> (A.special "static", tok)

and class_name_reference env = function
   | ClassNameRefStatic cn -> A.Id (class_name_or_selfparent env cn)
   | ClassNameRefDynamic (lv, []) -> lvalue env lv
   | ClassNameRefDynamic _ ->
       failwith "TODO ClassNameRefDynamic" (* of lvalue * obj_prop_access list *)

and lvalue env = function
  | Var (dn, scope) -> A.Id (dname dn)
  | This _ -> A.This
  | VArrayAccess (lv, (_, e, _)) ->
      let lv = lvalue env lv in
      let e = opt expr env e in
      A.Array_get (lv, e)
  | VArrayAccessXhp (e1, (_, e2, _)) ->
      let e1 = expr env e1 in
      let e2 = opt expr env e2 in
      A.Array_get (e1, e2)
  | VBrace (tok, (_, e, _)) ->
      A.Call (A.Id ((A.builtin "eval_var", tok)), [expr env e])
  | VBraceAccess (lv, (_, e, _)) ->
      A.Array_get (lvalue env lv, Some (expr env e))
  | Indirect (e, (Dollar tok)) ->
      A.Call (A.Id (A.builtin "eval_var", tok), [lvalue env e])
  | VQualifier (q, v)  ->
      A.Class_get (A.Id (qualifier env q),
                  A.Call (A.Id (A.builtin "eval_var",
                               Ast_php.fakeInfo (A.builtin "eval_var")),
                               [lvalue env v]))
  | ClassVar (q, dn) -> A.Class_get (A.Id (qualifier env q), A.Id (dname dn))
  | FunCallSimple (f, (_, args, _)) ->
      let f = name env f in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (A.Id f, args)
  | FunCallVar (q, lv, (_, argl, _)) ->
      let argl = comma_list argl in
      let argl = List.map (argument env) argl in
      let lv = lvalue env lv in
      let lv = match q with None -> lv | Some q -> A.Class_get (A.Id (qualifier env q), lv) in
      A.Call (lv, argl)
  | StaticMethodCallSimple (q, n, (_, args, _)) ->
      let f = A.Class_get (A.Id (qualifier env q), A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | MethodCallSimple (e, _, n, (_, args, _)) ->
      let f = lvalue env e in
      let f = A.Obj_get (f, A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | StaticMethodCallVar (lv, _, n, (_, args, _)) ->
      let f = A.Class_get (lvalue env lv, A.Id (name env n)) in
      let args = comma_list args in
      let args = List.map (argument env) args in
      A.Call (f, args)
  | StaticObjCallVar _ -> failwith "expr StaticObjCallVar"

  | ObjAccessSimple (lv, _, n) -> A.Obj_get (lvalue env lv, A.Id (name env n))
  | ObjAccess (lv, oa) ->
      let lv = lvalue env lv in
      obj_access env lv oa
  | DynamicClassVar (lv, _, lv2) ->
      A.Class_get (lvalue env lv, lvalue env lv2)


and obj_access env obj (_, objp, args) =
  let e = obj_property env obj objp in
  match args with
  | None -> e
  | Some (_, args, _) ->
      let args = comma_list args in
      let args = List.map (argument env) args in
      (* TODO CHECK THIS *)
      A.Call (e, args)

and obj_property env obj = function
  | ObjProp objd -> obj_dim env obj objd
  | ObjPropVar lv ->
      A.Call (A.Id (A.builtin "ObjPropVar",
                   Ast_php.fakeInfo (A.builtin "ObjPropVar")),
             [lvalue env lv])

and obj_dim env obj = function
  | OName n -> A.Obj_get (obj, A.Id(name env n))
  | OBrace (_, e, _) ->
      A.Obj_get (obj, expr env e)
  | OArrayAccess (x, (_, e, _)) ->
      let e = opt expr env e in
      let x = obj_dim env obj x in
      A.Array_get (x, e)
  | OBraceAccess _ -> failwith "TODO brace access"(*  of obj_dim * expr brace *)

and indirect env = function
  | Dollar _ -> failwith "expr Dollar" (* of tok *)

and argument env = function
  | Arg e -> expr env e
  | ArgRef (_, e) -> A.Ref (lvalue env e)

(* todo: use_traits! *)
and class_def env c =
  let _, body, _ = c.c_body in
  {
    A.c_type = class_type env c.c_type ;
    A.c_name = name env c.c_name;
    A.c_extends =
    (match c.c_extends with
    | None -> []
    | Some (_, x) -> [fst (name env x)]);
    A.c_traits =
      List.fold_right (class_traits env) body [];
    A.c_implements =
    (match c.c_implements with None -> []
    | Some x -> interfaces env x);
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
  List.map (fun x -> fst (name env x)) intfs

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
        (fst (name env n), static_scalar_affect env ss) :: acc
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
      let vis = visibility env m in
      let static = static env m in
      let abstract = abstract env m in
      let final = final env m in
      let ht = opt hint_type env ht in
      List.map (
        fun (n, ss) ->
          let name = fst (dname n) in
          let value = opt static_scalar_affect env ss in
          {
            A.cv_name = name;
            A.cv_value = value;
            A.cv_final = final; A.cv_static = static; A.cv_abstract = abstract;
            A.cv_visibility = vis;
            A.cv_type = ht;
          }
       ) cvl @ acc
  | _ -> acc

and visibility env = function
  (* juju: TODO CHECK, pad: ??? *)
  | [] -> A.Novis
  | Public :: _ -> A.Public
  | Private :: _ -> A.Private
  | Protected :: _ -> A.Protected
  | (Static | Abstract | Final) :: rl -> visibility env rl

and static env xs = List.mem Static xs
and abstract env xs = List.mem Abstract xs
and final env xs = List.mem Final xs

and class_body env st acc =
  match st with
  | Method md ->
      method_def env md :: acc
  | XhpDecl _ ->
      (* TODO failwith "TODO xhp decl" or we don't care and it's ok? *)
      acc
  | UseTrait _ ->
      acc
  | (ClassVariables (_, _, _, _)|ClassConstants (_, _, _)) -> acc

and method_def env m =
  let _, params, _ = m.m_params in
  let params = comma_list_dots params in
  let mds = List.map (fun (x, _) -> x) m.m_modifiers in
    { A.m_visibility = visibility env mds;
      A.m_static = static env mds;
      A.m_final = final env mds;
      A.m_abstract = abstract env mds;
      A.m_ref = (match m.m_ref with None -> false | Some _ -> true);
      A.m_name = name env m.m_name;
      A.m_params = List.map (parameter env) params ;
      A.m_return_type = opt hint_type env m.m_return_type;
      A.m_body = method_body env m.m_body;
    }

and method_body env = function
  | AbstractMethod _ -> []
  | MethodBody (_, stl, _) ->
      List.fold_right (stmt_and_def env) stl []

and parameter env p =
  { A.p_type = opt hint_type env p.p_type;
    A.p_ref = p.p_ref <> None;
    A.p_name = dname p.p_name;
    A.p_default = opt static_scalar_affect env p.p_default;
  }

and func_def env f =
  let _, params, _ = f.f_params in
  let params = comma_list_dots params in
  let _, body, _ = f.f_body in
  { A.f_ref = f.f_ref <> None;
    A.f_name = name env f.f_name;
    A.f_params = List.map (parameter env) params;
    A.f_return_type = opt hint_type env f.f_return_type;
    A.f_body = List.fold_right (stmt_and_def env) body [];
  }

and xhp_html env = function
  | Xhp (tag, attrl, _, body, _) ->
      let tag, _ = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = tag;
        A.xml_attrs = attrl;
        A.xml_body = List.map (xhp_body env) body;
      }
  | XhpSingleton (tag, attrl, _) ->
      let tag, _ = tag in
      let attrl = List.map (xhp_attribute env) attrl in
      { A.xml_tag = tag;
        A.xml_attrs = attrl;
        A.xml_body = [];
      }

and xhp_attribute env ((n, _), _, v) =
  n, xhp_attr_value env v

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
  | EncapsString (s, _) -> A.String s
  | EncapsVar v -> lvalue env v
  | EncapsCurly (_, lv, _) ->  lvalue env lv
  | EncapsDollarCurly (_, lv, _) -> lvalue env lv
  | EncapsExpr (_, e, _) -> expr env e

and array_pair env = function
  | ArrayExpr e -> A.Aval (expr env e)
  | ArrayRef (_, lv) -> A.Aval (A.Ref (lvalue env lv))
  | ArrayArrowExpr (e1, _, e2) -> A.Akval (expr env e1, expr env e2)
  | ArrayArrowRef (e1, _, _, lv) -> A.Akval (expr env e1, A.Ref (lvalue env lv))


and for_expr env el = List.map (expr env) (comma_list el)

and colon_stmt env = function
  | SingleStmt st -> stmt env st []
  | ColonStmt (_, stl, _, _) -> List.fold_right (stmt_and_def env) stl []
(*of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *) *)

and switch_case_list env = function
  | CaseList (_, _, cl, _) -> List.map (case env) cl
  | CaseColonList _ -> raise ObsoleteConstruct

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
  fst fq, fst dn, stdl

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
  | GlobalDollar _ -> failwith "TODO GlobalDollar" (*of tok * r_variable *)
  | GlobalDollarExpr _ -> failwith "TODO GlobalDollarExpr" (* of tok * expr brace *)

(*****************************************************************************)
(* For cmf *)
(*****************************************************************************)
let func_def x = func_def (empty_env()) x
let class_def x = class_def (empty_env()) x
let constant_def x = constant_def (empty_env()) x
