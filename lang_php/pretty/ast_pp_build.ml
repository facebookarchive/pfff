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

module A = Ast_pp
module PI = Parse_info
module T = Parser_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * The goal of this module is to take an AST (see ast_php.ml) and its
 * list of tokens (see parser_php.ml), including the newline and comment
 * tokens, and return an AST that will make it easy to pretty print
 * the code while still maintaining the comments of the original file
 * (see ast_pp.ml).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*
 * An environment contains all "esthetic" newlines and all the comments
 * of the current chunk of code we are processing.
 * It also contains the line numbers where the newlines/comments appear
 * in the code.
 *
 * An 'esthetic' newline is a newline used to better separate code.
 * For instance in
 *
 *    $x = 1;
 *    $x = 2;
 *
 * there is a newline, but we don't consider it an esthetic newline.
 * We don't consider those newlines because the pretty printer already knows
 * (and arguably knows better than the developer) where to insert them
 * in the code. Here is an example of an esthetic newline:
 *
 *    $x = 1;
 *
 *    $x = 2;
 *
 * See extract_esthetic_newlines_and_comments for more information.
 *
 * The environment is a ref that gets smaller as we build the ast_pp
 * from bottom to top (hence the use of fold_right in many places below).
 *)
type env = (int * [`Comment of string | `Newline ]) list ref

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

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _ :: rl -> last rl

let is_space =
  let re = Str.regexp "[\t \n]+" in
  fun x ->
  Str.string_match re x 0

let is_newline =
  let re = Str.regexp "[\t \n]*\n" in
  fun x ->
  Str.string_match re x 0

let ends_with_newline =
  let re = Str.regexp ".*\n" in
  fun x ->
  Str.string_match re x 0

(*****************************************************************************)
(* Newlines and comments helpers *)
(*****************************************************************************)

(* pad: why we need the last line of a stmt/toplevel/... ? *)

let rec last_info_of_stmt = function
  | If (_, _, _, _, Some (_, st)) -> last_info_of_stmt st
  | If (_, _, x, [], None) -> last_info_of_stmt x
  | If (_, _, _, l, None) ->
      let l = List.map (fun (_, _, x) -> x) l in
      last_info_of_stmt (last l)
  | While (_, _, (SingleStmt st)) -> last_info_of_stmt st
  | For (_, _, _, _, _, _, _, _, (SingleStmt st)) -> last_info_of_stmt st
  | Foreach (_, _, _, _, _, _, _, (SingleStmt st)) -> last_info_of_stmt st
  | Declare (_, _, (SingleStmt st)) -> last_info_of_stmt st
  | Try (_, _, (_, _, (_, _, x)), []) -> x
  | Try (_, _, _, l) ->
      let (_, _, (_, _, x)) = last l in
      x
  | While (_, _, (ColonStmt (_, _, _, x)))
  | For (_, _, _, _, _, _, _, _, (ColonStmt (_, _, _, x)))
  | Foreach (_, _, _, _, _, _, _, (ColonStmt (_, _, _, x)))
  | Declare (_, _, ColonStmt (_, _, _, x)) -> x
  | ExprStmt (_, x)
  | EmptyStmt x
  | Block (_, _, x)
  | IfColon (_, _, _, _, _, _, _, x)
  | Do (_, _, _, _, x)
  | Switch (_, _, (CaseList (_, _, _, x) | CaseColonList (_, _, _, _, x)))
  | Break (_, _, x)
  | Continue (_, _, x)
  | Return (_, _, x)
  | Throw (_, _, x)
  | Echo (_, _, x)
  | Globals (_, _, x)
  | StaticVars (_, _, x)
  | InlineHtml (_, x)
  | Use (_, _, x)
  | Unset (_, _, x)
  | TypedDeclaration (_, _, _, x)
   -> x

let last_line_of_stmt x = line_of_info (last_info_of_stmt x)

let rec last_line_of_stmtl = function
  | [] -> assert false
  | [st] -> last_line_of_stmt st
  | _ :: rl -> last_line_of_stmtl rl

let rec last_line_of_stmt_and_defl = function
  | [] -> assert false
  | [Stmt st] -> last_line_of_stmt st
  | [FuncDefNested { f_body = (_, _, x); _ } |
    ClassDefNested { c_body = (_, _, x); _ }
   ] -> line_of_info x
  | _ :: rl -> last_line_of_stmt_and_defl rl


(* pad: ?? *)
let rec pop_env stack line =
  match !stack with
  | [] -> None
  | (x, v) :: rl when x >= line ->
      stack := rl;
      Some v
  | _ -> None

let rec get_comments env acc line =
  match pop_env env line with
  | None   -> acc
  | Some x -> get_comments env (x :: acc) line

let rec add_comments convert env acc line =
  let comments = get_comments env [] line in
  let comments = List.map convert comments in
  comments @ acc


let add_stmt_comments = add_comments
    (function `Newline -> A.Newline | `Comment s -> A.Comment s)

let add_ce_comments = add_comments
    (function `Newline -> A.CEnewline | `Comment s -> A.CEcomment s)

let add_case_comments = add_comments
    (function `Newline -> A.Cnewline | `Comment s -> A.Ccomment s)



let make_env l =
  let l = List.map (fun (x, y) ->
    let tag =
      match y with
      | "\n" -> `Newline
      | x -> `Comment x
    in
    x, tag) l in
  let l = List.sort (fun (x, _) (y, _) -> y - x) l in
  ref l

let line_and_string_from_token x =
  let info = Token_helpers_php.info_of_tok x in
  let line = Parse_info.line_of_info info in
  let str  = Ast_php.str_of_info info in
  x, line, str

let rec extract_esthetic_newlines_and_comments l =
  let k = extract_esthetic_newlines_and_comments in
  match l with
  | [] -> []

  | (_, n1, v1) :: ((_, n2, v2) :: _ as rl)
    when ends_with_newline v1 && is_newline v2 ->
      (* pad: ?? *)
      if is_space v1
      then (n2, "\n") :: k rl
      else (n1, v1) :: k rl

  | (_, _, x) :: rl when is_space x -> k rl
  | (x, n, v) :: rl when Token_helpers_php.is_comment x -> (n, v) :: k rl
  (* the <?php is treated here as a kind of comment *)
  | (Parser_php.T_OPEN_TAG _, n, v):: rl -> (n, v) :: k rl
  | _ :: rl -> k rl

let env_of_tokens tokens =
  let tokens = List.map line_and_string_from_token tokens in
  let tokens = extract_esthetic_newlines_and_comments tokens in
  let env = make_env tokens in
  env

(* We will be called on chunk of code which is a little
 * bit different that when called on the whole code. For instance
 * on a program one can detect esthetic newlines between methods
 * by looking for 2 consecutive newlines, the one after the closing }
 * and the one after that. But with a chunk of code, there will be just
 * a single newline, the one preceding the method. Indeed the chunk will be
 * "\n  public function ....}\n".
 *)
let env_of_tokens_for_spatch toks =
  let toks =
    Common.exclude (function T.TSpaces _ -> true | _ -> false) toks in
  let l info = Ast_php.line_of_info info in
  let str info = Ast_php.str_of_info info in

  let rec aux ~start xs =
    match xs with
    | [] -> []
    | (T.T_COMMENT i1 | T.T_DOC_COMMENT i1 | T.T_OPEN_TAG i1)::xs ->
        (l i1, `Comment (str i1))::aux xs ~start:false
    (* two consecutive newlines, this is an esthetic newline *)
    | T.TNewline i1::T.TNewline i2::xs ->
        (l i1, `Newline)::aux (T.TNewline i2::xs) ~start:false

    | T.TNewline i1::xs when start ->
        (l i1, `Newline)::aux xs ~start:false

    | _::xs -> aux xs ~start:false
  in
  ref (List.rev (aux toks ~start:true))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec toplevel env st acc =
  match st with
  | StmtList stmtl ->
      let acc = List.fold_right (stmt env) stmtl acc in
      acc
  | FinalDef _ -> acc
  | FuncDef fd ->
      let _, _, end_ = fd.f_body in
      let acc = add_stmt_comments env acc (line_of_info end_) in
      A.FuncDef (func_def env fd) :: acc
  | ClassDef cd ->
      let _, _, end_ = cd.c_body in
      let acc = add_stmt_comments env acc (line_of_info end_) in
      A.ClassDef (class_def env cd) :: acc
  | ConstantDef cd -> raise Common.Todo
  | NotParsedCorrectly _ -> raise Common.Impossible

and stmt env st acc =
  let line = last_line_of_stmt st in
  let acc  = add_stmt_comments env acc line in
  let acc  = stmt_ env st acc in
  acc

and stmt_ env st acc =
  match st with
  | ExprStmt (e, _) ->
      let e = expr env e in
      A.Expr e :: acc
  | EmptyStmt _ -> A.Noop :: acc
  | Block (start, stdl, end_) ->
      let acc = List.fold_right (stmt_and_def env) stdl acc in
      let acc = add_stmt_comments env acc (line_of_info start) in
      acc
  | If (_, (_, e, _), st, il, io) ->
      let e = expr env e in
      let il = List.fold_right (if_elseif env) il (if_else env io) in
      let st = A.Block (stmt env st []) in
      let acc = A.If (e, st, il) :: acc in
      acc
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
  | Switch (_, (_, e, x), scl) ->
      let e = expr env e in
      let scl = switch_case_list env scl in
      let line = line_of_info x in
      let scl = add_case_comments env scl line in
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
  | Echo (_, el, _) ->
      A.Expr (A.Call (A.Id "echo", (List.map (expr env) (comma_list el)))) :: acc
  | Globals (_, gvl, _) -> A.Global (List.map (global_var env) (comma_list gvl)) :: acc
  | StaticVars (_, svl, _) ->
      A.StaticVars (List.map (static_var env) (comma_list svl)) :: acc
  | InlineHtml (s, _) -> A.InlineHtml s :: acc
  | Use (_, fn, _) -> A.Expr (A.Call (A.Id "use", [A.String (use_filename env fn)])) :: acc
  | Unset (_, (_, lp, _), e) ->
      let lp = comma_list lp in
      let lp = List.map (lvalue env) lp in
      A.Expr (A.Call (A.Id "unset", lp)) :: acc
  | Declare _ ->
      (* TODO failwith "stmt Declare" of tok * declare comma_list paren * colon_stmt *)
      acc
  | TypedDeclaration _ -> failwith "stmt TypedDeclaration" (* of hint_type * lvalue * (tok * expr) option * tok *)
  | IfColon _ -> failwith "This is old crazy stuff"


and use_filename env = function
  | UseDirect (s, _) -> s
  | UseParen (_, (s, _), _) -> s

and if_elseif env (_, (_, e, _), st) acc =
  let e = expr env e in
  let st = match stmt env st [] with [x] -> x | l -> A.Block l in
  A.If (e, st, acc)

and if_else env = function
  | None -> A.Noop
  | Some (_, (If _ as st)) ->
      (match stmt env st [] with
      | [x] -> x
      | l -> assert false)
  | Some (_, st) ->
      let acc = [] in
      let line = last_line_of_stmt st in
      let acc = add_stmt_comments env acc line in
      let acc = stmt env st acc in
      A.Block acc

and stmt_and_def env st acc =
  match st with
  | Stmt st -> stmt env st acc
  | FuncDefNested fd ->
      A.FuncDef (func_def env fd) :: acc
  | ClassDefNested cd ->
      A.ClassDef (class_def env cd) :: acc

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
(*      failwith "expr New" (* of tok * class_name_reference * argument comma_list paren option *) *)
  | Clone (_, e) ->
      A.Call (A.Id "clone", [expr env e])
  | AssignRef (e1, _, _, e2) ->
      let e1 = lvalue env e1 in
      let e2 = lvalue env e2 in
      A.Assign (None, e1, A.Ref e2)
  | AssignNew _ -> failwith "expr AssignNew" (* of lvalue * tok  * tok  * tok  * class_name_reference * argument comma_list paren option *)
  | Cast ((c, _), e) -> A.Cast (c, expr env e)
  | CastUnset _ -> failwith "expr CastUnset" (* of tok * expr  *)
  | InstanceOf (e, _, cn) ->
      let e = expr env e in
      let cn = class_name_reference env cn in
      A.InstanceOf (e, cn)
  | Eval (_, (_, e, _)) -> A.Call (A.Id "eval", [expr env e])
  | Lambda ld ->
      A.Lambda (lambda_def env ld)
  | Exit (_, e) ->
      let arg =
        match e with
        | None
        | Some (_, None, _) -> []
        | Some (_, Some e, _) -> [expr env e]
      in
      A.Call (A.Id "exit", arg)
  | At _ -> A.Id "@" (* failwith "expr At" (* of tok  *) TODO look at this *)
  | Print (_, e) ->
      A.Call (A.Id "print", [expr env e])
  | BackQuote (_, el, _) ->
      A.Call (A.Id "exec", [A.Guil (List.map (encaps env) el)])
(*      failwith "expr BackQuote" (* of tok * encaps list * tok *) *)
  | Include (_, e) ->
      A.Call (A.Id "include", [expr env e])
  | IncludeOnce (_, e) ->
      A.Call (A.Id "include_once", [expr env e])
  | Require (_, e) ->
      A.Call (A.Id "require", [expr env e])
  | RequireOnce (_, e) ->
      A.Call (A.Id "require_once", [expr env e])
  | Empty (_, (_, lv, _)) ->
      A.Call (A.Id "empty", [lvalue env lv])
  | Isset (_, (_, lvl, _)) ->
      A.Call (A.Id "isset", List.map (lvalue env) (comma_list lvl))
  | XhpHtml xhp -> A.Xhp (xhp_html env xhp)
  | Yield (_, e) -> A.Call (A.Id "yield", [expr env e])
  | YieldBreak _ -> A.Call (A.Id "yield", [A.Id "break"])
  | SgrepExprDots _ -> failwith "expr SgrepExprDots" (* of info *)
  | ParenExpr (_, e, _) -> expr env e

and lambda_def env ld =
  let _, params, _ = ld.l_params in
  let params = comma_list_dots params in
  let _, body, _ = ld.l_body in
  { A.l_ref = ld.l_ref <> None;
    A.l_params = List.map (parameter env) params;
    A.l_use =
      (match ld.l_use with
      | None -> []
      | Some (_tokuse, (_, lexical_vars, _)) ->
          let lexical_vars = comma_list lexical_vars in
          List.map (lexical_var env) lexical_vars
      );
    A.l_body = List.fold_right (stmt_and_def env) body [];
  }

and lexical_var env = function
  | LexicalVar (is_ref, name) ->
      { A.p_type = None;
        A.p_ref = is_ref <> None;
        A.p_name = dname name;
        A.p_default = None;
      }

and scalar env = function
  | C cst -> constant env cst
  | ClassConstant (q, s) -> A.Class_get (A.Id (qualifier env q), A.Id (name env s))
  | Guil (_, el, _) -> A.Guil (List.map (encaps env) el)
  | HereDoc ({ PI.token = PI.OriginTok x; _ },
             el,
             { PI.token = PI.OriginTok y; _ }) ->
      A.HereDoc (x.PI.str, List.map (encaps env) el, y.PI.str)
  | HereDoc _ -> assert false

and constant env = function
  | Int (n, _) -> A.Int n
  | Double (n, _) -> A.Double n
  | String (s, _) -> A.String s
  | CName n -> A.Id (name env n)
  | PreProcess (cpp, _) -> cpp_directive env cpp
  | XdebugClass _ -> failwith "stmt XdebugClass" (* of name * class_stmt list *)
  | XdebugResource -> failwith "stmt XdebugResource" (* *)

and cpp_directive env = function
  | Line      -> A.Id "__LINE__"
  | File      -> A.Id "__FILE__"
  | ClassC    -> A.Id "__CLASS__"
  | MethodC   -> A.Id "__METHOD__"
  | FunctionC -> A.Id "__FUNCTION__"
  | Dir       -> A.Id "__DIRECTORY__"
  | TraitC    -> A.Id "__TRAIT__"

and name env = function
  | Name (s, _) -> s
  | XhpName (rl, _) ->
      List.fold_left (fun x y -> x^":"^y) "" rl

and dname = function
  | DName (s, _) ->
      if s.[0] = '$' then s
      else "$"^s

and hint_type env = function
  | Hint q -> A.Hint (class_name_or_selfparent env q)
  | HintArray _ -> A.HintArray

and qualifier env (cn, _) = class_name_or_selfparent env cn

and class_name_or_selfparent env = function
   | ClassName fqcn -> name env fqcn
   | Self _ -> "self"
   | Parent _ -> "parent"
   (* todo: late static binding *)
   | LateStatic _ -> "static"

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
  | VBrace (_, (_, e, _)) ->
      A.Call (A.Id "eval_var", [expr env e])
  | VBraceAccess (lv, (_, e, _)) ->
      A.Array_get (lvalue env lv, Some (expr env e))
  | Indirect (e, _) ->
      A.Call (A.Id "eval_var", [lvalue env e])
  | VQualifier (q, v)  ->
      A.Class_get (A.Id (qualifier env q), A.Call (A.Id "eval_var", [lvalue env v]))
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
  | StaticObjCallVar _ -> failwith "expr StaticObjCallVar" (* of lvalue * tok (* :: *) * lvalue * argument comma_list paren *)

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
      A.Call (A.Id "ObjPropVar", [lvalue env lv])

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

and class_def env c =
  let _, body, _ = c.c_body in
  let acc = List.fold_right (class_body env) body [] in
  let line = line_of_info (info_of_name c.c_name) in
  let acc = add_ce_comments env acc line in
  {
    A.c_type = class_type env c.c_type ;
    A.c_name = name env c.c_name;
    A.c_extends =
    (match c.c_extends with
    | None -> []
    | Some (_, x) -> [name env x]);
    A.c_implements =
    (match c.c_implements with None -> []
    | Some x -> interfaces env x);
    A.c_body = acc;
  }

and class_type env = function
  | ClassRegular _  -> A.ClassRegular
  | ClassFinal _    -> A.ClassFinal
  | ClassAbstract _ -> A.ClassAbstract
  | Interface _ -> A.Interface
  | Trait _ -> A.Trait

and interfaces env (_, intfs) =
  let intfs = comma_list intfs in
  List.map (name env) intfs

and static_scalar_affect env (_, ss) = static_scalar env ss
and static_scalar env a = expr env a

and class_variables env st acc =
  match st with
  | _ -> acc

and visibility env = function
  | [] -> (* TODO CHECK *) A.Novis
  | Public :: _ -> A.Public
  | Private :: _ -> A.Private
  | Protected :: _ -> A.Protected
  | (Static | Abstract | Final) :: rl -> visibility env rl

and static env = function
  | [] -> false
  | Static :: _ -> true
  | _ :: rl -> static env rl

and abstract env = function
  | [] -> false
  | Abstract :: _ -> true
  | _ :: rl -> abstract env rl

and final env = function
  | [] -> false
  | Final :: _ -> true
  | _ :: rl -> final env rl

and class_body env st acc =
  match st with
  | Method md ->
      let x = match md.m_body with
      | AbstractMethod x -> x
      | MethodBody (_, _, x) -> x in
      let line = line_of_info x in
      let acc  = add_ce_comments env acc line in
      let acc  = A.CEmethod (method_def env md) :: acc in
      acc
  | ClassVariables (m, ht, cvl, x) ->
      let line = line_of_info x in
      let acc  = add_ce_comments env acc line in
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
      let vars = List.map (
        fun (n, ss) ->
          dname n, opt static_scalar_affect env ss
       ) cvl in
      let cv = {
        A.cv_final = final;
        A.cv_static = static;
        A.cv_abstract = abstract;
        A.cv_visibility = vis;
        A.cv_type = ht;
        A.cv_vars = vars;
        } in
      let acc = A.CEdef cv :: acc in
      let line = line_of_info (info_of_dname (fst (List.hd cvl))) in
      let acc = add_ce_comments env acc line in
      acc
  | ClassConstants (_, cl, _) ->
      let consts =
        List.map (
        fun (n, ss) ->
          (name env n, static_scalar_affect env ss)
       ) (comma_list cl) in
      A.CEconst consts :: acc
  | XhpDecl _ -> acc(* TODO failwith "TODO xhp decl" *)(* of xhp_decl *)
  | UseTrait _ -> failwith "TODO: UseTrait"

and method_def env m =
  let acc = [] in
  let body = m.m_body in
  let acc = method_body env body acc in
  let line = line_of_info (info_of_name m.m_name) in
  let acc = add_stmt_comments env acc line in
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
      A.m_body = acc;
    }

and method_body env x acc =
  match x with
  | AbstractMethod _ -> acc
  | MethodBody (_, stl, _) ->
      List.fold_right (stmt_and_def env) stl acc

and parameter env p =
  { A.p_type = opt hint_type env p.p_type;
    A.p_ref = p.p_ref <> None;
    A.p_name = dname p.p_name;
    A.p_default = opt static_scalar_affect env p.p_default;
  }

and func_def env f =
  let acc = [] in
  let _, body, end_ = f.f_body in
  let acc = add_stmt_comments env acc (line_of_info end_) in
  let acc = List.fold_right (stmt_and_def env) body acc in
  let line = line_of_info (info_of_name f.f_name) in
  let acc = add_stmt_comments env acc line in
  let _, params, _ = f.f_params in
  let params = comma_list_dots params in
  { A.f_ref = f.f_ref <> None;
    A.f_name = name env f.f_name;
    A.f_params = List.map (parameter env) params;
    A.f_return_type = opt hint_type env f.f_return_type;
    A.f_body = acc;
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
      A.AttrString (List.map (encaps env) l)
  | XhpAttrExpr (_, e, _) ->
      A.AttrExpr (expr env e)
  | SgrepXhpAttrValueMvar _ -> raise Common.Impossible

and xhp_body env = function
  | XhpText (s, _) -> A.XhpText s
  | XhpExpr (_, e, _) -> A.XhpExpr (expr env e)
  | XhpNested xml -> A.XhpXml (xhp_html env xml)

and encaps env = function
  | EncapsString (s, _) -> A.EncapsString s
  | EncapsVar v -> A.EncapsVar (lvalue env v)
  | EncapsCurly (_, lv, _) -> A.EncapsCurly (lvalue env lv)
  | EncapsDollarCurly (_, lv, _) -> A.EncapsDollarCurly (lvalue env lv)
  | EncapsExpr (_, e, _) -> A.EncapsExpr (expr env e)

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
  | CaseList (_, _, cl, _) ->
      List.fold_right (case env) cl []
  | CaseColonList _ -> failwith "What's that?"
(*      tok (* : *) * tok option (* ; *) * case list *
        tok (* endswitch *) * tok (* ; *) *)

and case env x acc =
  match x with
  | Case (_, e, _, []) -> A.Case (expr env e, []) :: acc
  | Case (_, e, _, stl) ->
      let line = last_line_of_stmt_and_defl stl in
      let acc = add_case_comments env acc line in
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Case (expr env e, stl) :: acc
  | Default (_, _, stl) ->
      let line = last_line_of_stmt_and_defl stl in
      let acc = add_case_comments env acc line in
      let stl = List.fold_right (stmt_and_def env) stl [] in
      A.Default stl :: acc

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
  fq, dn, stdl

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
(* Entry points used by prettyphp *)
(*****************************************************************************)

let program_with_comments tokens top_l =
  let env = env_of_tokens tokens in
  let acc = [] in
  let acc = List.fold_right (toplevel env) top_l acc in
  let acc = add_stmt_comments env acc 0 in
  acc

(*****************************************************************************)
(* Entry points used by spatch *)
(*****************************************************************************)

let toplevels tokens top_l =
  let env = env_of_tokens_for_spatch tokens in
  let acc = [] in
  let acc = List.fold_right (toplevel env) top_l acc in
  let acc = add_stmt_comments env acc 0 in
  acc

let class_stmts tokens xs =
  let env = env_of_tokens_for_spatch tokens in
  let acc = [] in
  let acc = List.fold_right (class_body env) xs acc in
  let acc = add_ce_comments env acc 0 in
  acc

