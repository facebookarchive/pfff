(*s: pil_build.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common 

open Ast_php

module Ast = Ast_php

module A = Ast_php
module B = Pil

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The goal of this module is to transform constructs from Ast_php to
 * the simpler constructs of Pil. Here are the simplifications done:
 * 
 * - linearization of expression
 * - sugar removal for postfix/infix increments
 * - removing most tokens from the AST, to have really an AST, not a CST
 *   (keep the one associated with names as error reporting may need
 *    this information)
 * - TODO many other stuff
 * 
 *)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let noType () = ({ B.t = [Type_php.Unknown] })

let mkt x = x, noType ()

let counter = ref 0
let fresh_var () = 
  incr counter;
  let s = spf "F_%d" !counter in
  B.Var (A.DName (s, Ast.fakeInfo s))

let vv_of_v v=
  mkt (B.VVar v)

let fresh_vvar () = 
  vv_of_v (fresh_var ())

(* there are a few cases as when we transform expressions where we need
 * to build constant integers, for instance when $i++ -> $i = $i + !
 *)
let expr_of_int i =
  mkt(B.C(A.Int(string_of_int i, A.fakeInfo (string_of_int i))))

(* when transforming certain loops we need to generate fresh variables
 * initialized to a certain boolean value (like $done = false; while (!$done)
 *)
let expr_true () =
  mkt(B.C(A.CName(A.Name("true", A.fakeInfo "true"))))
let expr_false () =
  mkt(B.C(A.CName(A.Name("false", A.fakeInfo "false"))))


let stmt_of_stmts_list = function
| [] -> B.EmptyStmt
| [stmt] -> stmt
| l -> B.Block l

let instrs_to_stmts xs = 
  xs +> List.map (fun x -> B.Instr x)

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Expressions *)
(* ------------------------------------------------------------------------- *)

(* given some expressions like 'foo() + bar()' we will return 
 * a set of instructions, here $tmp1 = foo(); $tmp2 = bar(); $tmp3=$tmp1+$tmp2;
 * as well as the resultant expressions, here the simple lvalue $tmp3
 *)
let (linearize_expr: A.expr -> B.instr list * B.expr) = fun e ->

  let instrs = ref [] in

  let rec (aux_lvalue: A.lvalue -> B.lvalue) = fun v ->
    match Ast.untype v with
    | A.Var (dname, _scoperef) -> mkt (B.VVar (B.Var dname))

    | A.FunCallSimple (name, argsA) ->
        (* note that both aux_args and make_var_from_call also modify
         * by side effect 'instrs'
         *)
        let argsB = argsA |> Ast.unparen |> Ast.uncomma |> aux_args in
        let v = make_var_from_call (B.SimpleCall name) argsB in
        vv_of_v v

    | A.ObjAccessSimple (obj, _, name) ->
        let v = make_var_from_Alvalue obj in
        mkt (B.ObjAccess(v, name))

    | A.MethodCallSimple (obj, _, name, argsA) ->
        let v_obj = make_var_from_Alvalue obj in
        let argsB = argsA |> Ast.unparen |> Ast.uncomma |> aux_args in
        let v = make_var_from_call (B.MethodCall(v_obj, name)) argsB in
        vv_of_v v

    | A.StaticMethodCallSimple (qualif, name, argsA) ->
        let argsB = argsA |> Ast.unparen |> Ast.uncomma |> aux_args in
        let v = make_var_from_call (B.StaticMethodCall(qualif, name)) argsB in
        vv_of_v v

    | A.ObjAccess (obj, (_, obj_prop, argsA_opt)) ->
        let v_obj = make_var_from_Alvalue obj in
        (match obj_prop, argsA_opt with
         | ObjPropVar(att), None ->
            (* example: $obj->$att=3 *)
            let v_att = make_var_from_Alvalue att in
            mkt(B.DynamicObjAccess(v_obj, v_att))
         | ObjPropVar(att), Some argsA ->
            let v_att = make_var_from_Alvalue att in
            let argsB = argsA |> Ast.unparen |> Ast.uncomma |> aux_args in
            let v = make_var_from_call 
                    (B.DynamicMethodCall(v_obj, v_att)) argsB in
            vv_of_v v
         | ObjProp(obj_dim), _ ->
            (* for this case we need a special recursive function
             * since an obj_dim can contain another obj_dim *)
             let argsB_opt = 
               argsA_opt |> Common.map_option 
                 (fun args -> args |> Ast.unparen |>
                                Ast.uncomma |> aux_args) in
               lv_of_obj_dim v_obj obj_dim argsB_opt
        )

    | A.FunCallVar (qualif_opt, func, argsA) ->
        let v_func = make_var_from_Alvalue func in
        let argsB = argsA |> Ast.unparen |> Ast.uncomma |> aux_args in
        let v_res =
          (* TODO: can do better than a dynamic call *)
          make_var_from_call
          (B.DynamicCall(qualif_opt, v_func)) argsB in
        vv_of_v v_res

    | A.VQualifier(qualif, lv) ->
        let v = make_var_from_Alvalue lv in
        mkt (B.VQualifier(qualif, v))

    | A.This t -> mkt (B.VVar (B.This t))

    | A.VArrayAccess(lvA, e) ->
        (* we need to pass by reference because of the case:
         * $a[1][2]=3;
         * $b=$a[1];
         * var_dump($b); // 3
         * $b[2]=0;
         * var_dump($b); // 0
         * var_dump($a); // still 3!! Because of copy on write
         * 
         * todo? or we could not care about the copy-on-write thing
         * and simplify even more the PIL. Maybe most of the static
         * analysis we need are not dependent on the copy-on-write
         * semantic.
         *)
        let lvB = aux_lvalue lvA in
        let v = fresh_var() in
        let vv = vv_of_v v in
        let instr = B.AssignRef(vv, lvB) in
        Common.push2 instr instrs;
        let expr = Common.map_option aux_expr (unbracket e) in
        mkt (B.ArrayAccess(v, expr))

    | A.VBraceAccess(lv, e_brace) ->
        let t1, e, t2 = e_brace in
        let e_bracket_opt = t1, Some(e), t2 in
        aux_lvalue (A.VArrayAccess(lv, e_bracket_opt), snd v)

    | A.Indirect(lv, indir) ->
        let v = make_var_from_Alvalue lv in
        mkt (B.IndirectAccess(v, indir))

    | A.VBrace(_, e_brace) ->
        let e = A.unbrace e_brace in
        make_var_from_Aexpr e |> vv_of_v

  and aux_args args = 
    args +> List.map (function
    | A.Arg e -> 
        let e = aux_expr e in
        B.Arg e
    | A.ArgRef (_tok, var) ->
        let lv = aux_lvalue var in
        B.ArgRef lv
    )

  and aux_expr e = 
      match Ast.untype e with
      | A.Lv v -> 
          let v = aux_lvalue v in
          mkt (B.Lv v)
      | A.Assign (v, _tok, e) ->
          let v = aux_lvalue v in
          let e = aux_expr e in
          let instr = B.Assign (v, B.AssignEq, e) in
          Common.push2 instr instrs;
          mkt (B.Lv v)
      | A.Binary (e1, (op, _wrap), e2) ->
          let e1 = aux_expr e1 in
          let e2 = aux_expr e2 in
          mkt (B.Binary (e1, op, e2))

      | A.Unary (op, eA) ->
          let eB = aux_expr eA in
          mkt (B.Unary(A.unwrap op, eB))

      | A.Sc sc ->
          (match sc with
          | A.C cst ->
              mkt (B.C cst)
          | A.ClassConstant (q, n) ->
              mkt (B.ClassConstant (q, n))
          | (HereDoc (_, _, _)|Guil (_, _, _)) ->
              raise Todo
          )

      | A.ConsArray (_tok, array_pairs) ->
          (* TODOjjeannin: for the cases ArrayRef and ArrayArrowRef
           * the translation is semantically wrong, but it's good enough
           * for type inference and control flow analysis 
           * 
           * For mixed arrows/not arrows arrays, the following example 
           * gives a good idea of the semantics:
           * php> var_dump(array(1,"car"=>2,3,"bus"=>4,5));
           * array(5) {
           *   [0]=>
           *   int(1)
           *   ["car"]=>
           *   int(2)
           *   [1]=>
           *   int(3)
           *   ["bus"]=>
           *   int(4)
           *   [2]=>
           *   int(5)
           * }
           *)
          let array_pairs = array_pairs +> Ast.unparen +> Ast.uncomma in

          if array_pairs +> List.exists (function
          | A.ArrayArrowExpr _ | A.ArrayArrowRef _ -> true
          | _ -> false
          ) 
          then
            let i = ref 0 (* next index in the array *) in
            let exprs = 
              array_pairs +> List.map (function
              | A.ArrayExpr e -> 
                  let res = expr_of_int !i, aux_expr e in
                    i:= !i+1; res
              | A.ArrayArrowExpr (e1, _, e2) ->
                  aux_expr e1, aux_expr e2
             (* the last two cases are semantically wrong (I'm suppressing &)*)
              | A.ArrayRef (_, lv) ->
                  let res = expr_of_int !i, mkt(B.Lv(aux_lvalue lv)) in
                    i:= !i+1; res
              | A.ArrayArrowRef (e1, _, _, lv2) ->
                  aux_expr e1, mkt(B.Lv(aux_lvalue lv2))
              )
            in mkt (B.ConsHash exprs)
          else
            let exprs =
              array_pairs +> List.map (function
              | A.ArrayExpr e -> aux_expr e
              | A.ArrayRef(_, lA) -> (* semantically wrong *)
                  let lB = aux_lvalue lA in
                  mkt(B.Lv lB)
              | _ -> raise Impossible
              )
            in mkt (B.ConsArray exprs)

      (* no need to care about extra tokens *)
      | A.ParenExpr e_paren ->
          e_paren |> A.unparen |> aux_expr

      | A.AssignOp(lvA, op_w, eA) ->
          let lvB = aux_lvalue lvA in
          let eB = aux_expr eA in
          let instr = B.Assign (lvB, B.AssignOp(A.unwrap op_w), eB) in
          Common.push2 instr instrs;
          mkt (B.Lv lvB)

      | A.Cast (c, e) ->
          mkt (B.Cast (A.unwrap c, aux_expr e))

      | A.CondExpr(cond, _, if_t, _, if_f) ->
          mkt (B.CondExpr(aux_expr cond, aux_expr if_t, aux_expr if_f))

      | A.Isset (t, argsA) ->
          (* for now we transform it into a normal function call ; TODO *)
          (* this is wrong because the argument is going to be evaluated *)
          let argsB = argsA |> A.unparen |> A.uncomma |>
                      (List.map (fun a -> B.Arg(mkt(B.Lv( aux_lvalue a))))) in
          let v = make_var_from_call 
                    (B.SimpleCall (Name("isset", A.fakeInfo "isset"))) argsB in
          mkt (B.Lv (vv_of_v v))

      | A.Print (t, e) ->
          (* we transform it into a normal function call *)
          let v = make_var_from_call (B.SimpleCall 
                                        (Name("print", A.fakeInfo "print"))) 
                    [ B.Arg(aux_expr e) ] in
          mkt (B.Lv (vv_of_v v))

      | A.Empty (t, lv) ->
          (* we transform it into a normal function call *)
          let v = make_var_from_call (
            B.SimpleCall (Name("empty", A.fakeInfo "empty"))) 
                    [ B.Arg(mkt(B.Lv(aux_lvalue(A.unparen lv)))) ] in
          mkt (B.Lv (vv_of_v v))

      | A.New(_, cnr, args_opt) ->
          (* if args_opt is None I call a function with an empty list
           * not sure that's right *)
          let v = make_var_from_call
                (B.New cnr)
                (match args_opt with 
                   |None -> [] 
                   |Some(args)-> args |> A.unparen |> A.uncomma |> aux_args) 
          in
          mkt (B.Lv (vv_of_v v))

      | A.AssignNew(lvA, _, _, _, cnr, args_opt) -> 
          let lvB = aux_lvalue lvA in
          let argsB =
            (match args_opt with 
               |None -> [] 
               |Some(argsA)-> argsA |> A.unparen |> A.uncomma |> aux_args) in
          let instr = B.Call (lvB, B.New cnr, argsB) in
          Common.push2 instr instrs;
          mkt (B.Lv lvB)

      | A.At(_, expr) ->
          (* for now we just suppress the @ signs *)
          aux_expr expr

      | A.InstanceOf(expr, _, cnr) ->
          mkt(B.InstanceOf(aux_expr expr, cnr))

      | A.Infix(op_w, lvA) | A.Postfix(lvA, op_w) ->
          (* ++lv returns the final value of lv
           * lv++ returns the initial value of lv
           *
           * For any context C[.] :
           * C[++lv] should be translated as 
           * lv=lv+1; fresh=lv; C[lv] 
           * because lv might be changed again in the context C 
           *
           * C[lv++] should be translated as
           * fresh=lv; lv=lv+1; C[lv]
           *)
          let binop = Arith(
            match A.unwrap op_w with Dec -> Plus | Inc -> Minus) in
          let lvB = aux_lvalue lvA in
          let instr = B.Assign(lvB, B.AssignEq, 
                      mkt(B.Binary(mkt(B.Lv(lvB)), binop, 
                                   expr_of_int 1))) in
          let fresh =
          (match Ast.untype e with 
           | A.Infix(_,_) -> 
               Common.push2 instr instrs; (* pushing lv=lv+1 *)
               make_var_from_Blvalue lvB        
                   (* creating and pushing fresh=lv *)
           | A.Postfix(_,_) ->
               let fresh0 = make_var_from_Blvalue lvB in 
                   (* creating/push fresh=lv *)
               Common.push2 instr instrs;         (* pushing lv=lv+1 *)
               fresh0
           | _ -> raise Impossible
          ) in mkt (B.Lv (vv_of_v fresh))  (* call to fresh inside context *)

      | A.AssignRef(lv1, _, _, lv2) ->
          let lv1B = aux_lvalue lv1 in
          let lv2B = aux_lvalue lv2 in
          let instr = B.AssignRef (lv1B, lv2B) in
          Common.push2 instr instrs;
          mkt (B.Lv lv1B)

      | A.AssignList(_, la, _, expr) ->
          (* list($la1, $la2, ..., $lan) = expr has to be translated as 
           * $fresh = expr; $la1 = fresh[1]; 
           * $la2 = fresh[2]; ...; $la<n> = fresh[n]; $fresh
           * if $la<i> is a list itself list($la<i>1, ..., $la<i>m), 
           * we need to replace the instruction
           * $la<i>=fresh by the instrucions:
           * $la<i>1 = fresh[i][1]; ...; $la<i>m = fresh[i][m]
           * and so on recursively *)
          let fresh = make_var_from_Aexpr expr in (* $fresh = expr *)
          push_list_assign la fresh;
          mkt(B.Lv(vv_of_v fresh))


      | (RequireOnce (_, _)|Require (_, _)|IncludeOnce (_, _)|Include (_, _))
        -> failwith "Those should be lifted by another simplification phase"

      | A.XhpHtml _ -> raise Todo

      | A.Eval(_, e_paren) ->
          (*let v = e_paren |> A.unparen |> make_var_from_Aexpr in
          mkt (B.Lv( vv_of_v v))*) 
          raise Todo

      | A.EDots _ -> 
          raise Impossible
      | A.Lambda _ -> 
          raise Todo

      | A.Exit (_, args) -> raise Todo
      |A.BackQuote (_, _, _)|A.CastUnset (_, _)|A.Clone (_, _)
        -> raise Todo
    
  (* Helper functions for converting lvalues, expressions and calls into
   * variables; their use ensures that we never forget the Common.push2 
   *)
  and (make_var_from_Blvalue: B.lvalue -> B.var) = fun lvB ->
    let v = fresh_var() in
    let vv = mkt (B.VVar v) in
    let instr = B.Assign (vv, B.AssignEq, mkt (B.Lv lvB)) in
    Common.push2 instr instrs;
    v

  and (make_var_from_Alvalue: A.lvalue -> B.var) = fun lvA ->
    let (lvB: B.lvalue) = aux_lvalue lvA in 
    make_var_from_Blvalue lvB

  and (make_var_from_Bexpr: B.expr -> B.var) = fun eB ->
    let v = fresh_var () in
    let vv = mkt (B.VVar v) in
    let instr = B.Assign (vv, B.AssignEq, eB) in
    Common.push2 instr instrs;
    v

  and (make_var_from_Aexpr: A.expr -> B.var) = fun eA ->
    let (eB: B.expr) = aux_expr eA in
    make_var_from_Bexpr eB

  and (make_var_from_call: B.call_kind -> B.argument list -> B.var) = 
    fun ck args ->
    let v = fresh_var() in
    let instr = B.Call (vv_of_v v, ck, args) in
    Common.push2 instr instrs;
    v

  and (lv_of_obj_dim: 
         B.var -> A.obj_dim -> B.argument list option -> B.lvalue) = 
    fun v_obj obj_dim argsB_opt -> 
      match obj_dim with
      (* TODOjjeannin: better explanation of what's going on
       * var_of_obj_dim obj_var obj_dim args translates
       * $obj_var->obj_dim(args), i.e. in the AST of PHP
       * ObjAccess(Var($obj_var), ObjProp(obj_dim))
       * where $obj_var is a variable for the object that has been
       * created previously *)
    | OName(name) -> 
        (match argsB_opt with
          | None -> mkt(B.ObjAccess(v_obj, name))
          | Some(argsB) ->
              let v = make_var_from_call
                      (B.MethodCall(v_obj, name)) argsB in
              vv_of_v v
        )
    | OBrace (e_brace) ->
        (* example: $obj->{expr}=3; or $obj->{expr}(args)=3; *)
        begin
          let e = A.unbrace e_brace in
          match argsB_opt with
          | None ->  mkt(B.DynamicObjAccess(v_obj, make_var_from_Aexpr e))
          | Some(argsB) -> 
              let v = make_var_from_call 
              (B.DynamicMethodCall(v_obj, make_var_from_Aexpr e)) argsB in
              vv_of_v v
        end
    | OArrayAccess (od, e_brace) -> 
        (* example: $obj->att[1]=3 or $obj->att[1](args)=3; *)        
        let lv_od = lv_of_obj_dim v_obj od None in
        let v_od = make_var_from_Blvalue lv_od in
        let e = e_brace |> A.unbracket |> (Common.map_option aux_expr) in
        let v = make_var_from_Blvalue (mkt(B.ArrayAccess(v_od, e))) in
        begin 
          match argsB_opt with
          | None -> vv_of_v v 
          | Some(argsB) ->
              let v2 = make_var_from_call
                       (B.DynamicCall(None, v)) argsB in
              vv_of_v v2
        end
    | OBraceAccess (od, e_brace) ->
        (* example: $obj->att{1}=3; or $obj->att{1}(args)=3; *)
        (* the semantic of { } in that case is exactly the same as [ ] *)
        let t1, e, t2 = e_brace in
        let e_bracket_opt = t1, Some(e), t2 in
        lv_of_obj_dim v_obj (A.OArrayAccess(od, e_bracket_opt)) argsB_opt

  and (push_list_assign :
         A.list_assign A.comma_list A.paren -> B.var -> unit)
          (* (push_list_assign list(la1, ..., lan) var) pushes on instrs
           * the assignments la1=var[1]; ...; la2=var[2]
           * needs to be called recursively if some la<i> is itself
           * a list(la<i>1, ..., la<i>m) *)
       = fun la_cp var -> 
         let i = ref 0 in (* counter in the list, starts at 0 *)
         la_cp |> A.unparen |> A.uncomma |> List.iter
           ( function
             | ListVar(lvA) ->
                 let lvB = aux_lvalue lvA in
                 (* $lvB = $var[i] *) 
                 let instr = B.Assign(
                   lvB, B.AssignEq,
                   mkt(B.Lv(mkt(B.ArrayAccess(var,
                       Some(expr_of_int !i)))))) in
                 i:= !i+1;
                 Common.push2 instr instrs
             | ListEmpty -> i:= !i+1
             | ListList(_, la2)->
                 (* needs an assignment by reference
                  * to see why, see the A.VArrayAccess case *)
                 (* $var2 =& $var[i]
                  * then recursive call *)
                 let lvB = 
                   B.ArrayAccess(var,
                                 Some(expr_of_int !i))
                 in
                 i:= !i+1;
                 let var2 = fresh_var() in
                 let instr = B.AssignRef(vv_of_v var2, mkt lvB) in
                 Common.push2 instr instrs;
                 push_list_assign la2 var2
           )

  in
  let last_expr = aux_expr e in

  List.rev (!instrs), last_expr
  
(* ------------------------------------------------------------------------- *)
(* Statements *)
(* ------------------------------------------------------------------------- *)

(* a single statement like while(foo()) { } will now generate multiple
 * PIL statements, here  $tmp = foo(); while($tmp) {  $tmp = foo(); }
 *)
let (linearize_stmt: Ast_php.stmt -> Pil.stmt list) = fun st ->

  let rec (aux_stmt : A.stmt -> B.stmt list) = fun st ->
    match st with
    | A.ExprStmt (e, _tok) ->
        (* todo? print warning of _last_expr is not
         * a simple Lv ? 
         *)
        let instrs, _last_expr = linearize_expr e in
        instrs_to_stmts instrs

    | A.Block stmt_and_defs ->
        let xs = Ast.unbrace stmt_and_defs in
        xs +> List.map aux_stmt_and_def +> List.flatten

    | A.Echo (_, exprs, _) ->
        let exprs = Ast.uncomma exprs in

        let xs = List.map linearize_expr exprs in
        let all_instrs = List.map fst xs +> List.flatten in
        let all_expr = List.map snd xs in
        
        instrs_to_stmts all_instrs ++ [B.Echo all_expr]

    | A.Return (_, eopt, _) ->
        (* todo: should be more readable, a monad or fmap_xxx *)
        let eopt, instrs = 
          match eopt with
          | None -> None, []
          | Some e -> 
              let instrs, last_expr = linearize_expr e in
              Some last_expr, instrs
        in
        instrs_to_stmts instrs ++ [B.Return eopt]

    | A.While (_tok, expr, colon_stmt) ->
        let expr = Ast.unparen expr in
        let instrs, last_expr = linearize_expr expr in
        let setup_expr_stmts = instrs_to_stmts instrs in

        let stmts = aux_colon_stmt colon_stmt in
        setup_expr_stmts ++ [B.While (last_expr, 
                                     B.Block (stmts ++ setup_expr_stmts))]

    (* do{ A } while (b) is translated as
     * $fresh = true; while($fresh) do { A; $fresh = b }
     *)
    | A.Do(_, stmt, _, expr, _) ->
        let expr = Ast.unparen expr in
        let stmts = aux_stmt stmt in

        let instrs, last_expr = linearize_expr expr in
        let setup_expr_stmts = instrs_to_stmts instrs in
        
        let fresh = fresh_vvar() in
        let fresh_true = B.Instr(B.Assign(fresh, B.AssignEq, expr_true()))
        (* $fresh = true *) in
        let fresh_b = B.Instr(B.Assign(fresh, B.AssignEq, last_expr))
                        (* $fresh = b *) in
        [ fresh_true ] ++ [ B.While (mkt(B.Lv(fresh)),
          B.Block(stmts ++ setup_expr_stmts ++ [fresh_b])) ]

    (* for(init1, ..., initk; cond1, ..., condm; incr1, ..., incrn)
     * { stmt } is translated as
     * init1; ...; initk; cond1; ...; cond<m-1>; while(condm) 
     * { stmt; incr1; ...; incrn ; cond1; ...; cond<m-1> }
     * see http://php.net/manual/en/control-structures.for.php
     *)
    | A.For(_, _, initA, _, condA, _, incrA, _, stmtA) -> 
        let rec (list_to_expr_cond : 
           (B.instr list * B.expr) list -> B.instr list * B.expr) = function
        (* from a list [instrs1,expr1;...;instrsn,exprn]
         * builds the pair
         * [instrs1; expr1; instrs2... instrs<n-1>; expr<n-1>; instrsn], exprn
         *)
         | [] -> [], expr_true()
         | [instrs, expr] -> instrs, expr
         | (instrs, expr)::tl -> 
             let instrs_tl, expr_tl = list_to_expr_cond tl in
             instrs ++ [B.Eval(expr)] ++ instrs_tl, expr_tl
        in let rec (flatten_instrs :
            (B.instr list * B.expr) list -> B.instr list) = function
        (* from a list [instr1,expr1;...;instrn,exprn]
         * builds the list
         * [instr1; expr1; instr2... instr<n-1>; expr<n-1>; instrn; exprn ]
         *)
         | [] -> []
         | (instrs, expr)::tl ->
             let instrs_tl = flatten_instrs tl in
             instrs ++ [B.Eval(expr)] ++ instrs_tl
        in
        let init_stmts = initA |> A.uncomma |> List.map linearize_expr
          |> flatten_instrs |> instrs_to_stmts
        and cond_instrs, cond = condA |> A.uncomma 
          |> List.map linearize_expr |> list_to_expr_cond
        and incr_stmts = incrA |> A.uncomma |> List.map linearize_expr
          |> flatten_instrs |> instrs_to_stmts
        and stmtB = aux_colon_stmt stmtA in
        let cond_stmts = cond_instrs |> instrs_to_stmts in
          init_stmts ++ cond_stmts ++ 
          [ B.While(cond, B.Block( stmtB ++ incr_stmts ++ cond_stmts )) ]

    (* foreach($arr as $value){stmt} is translated as
     * while(list(, $value) = each($arr)){stmt}, i.e.
     *
     *
     * foreach($arr as $key => $value){stmt} is translated as
     * while(list($key, $value) = each($arr)){stmt}, i.e.
     *)
    | A.Foreach(_, _, expr, _, lv, arr, _, stmt) -> 
        (* According to Drew Parovski, this will not work
         * as the each construct uses an internal temp variable
         * so if we translate like this any nested foreach loop
         * will have the wrong semantics; still according to him
         * foreach cannot be translated only using the construcs of PIL 
         *)
        raise Todo

    (* if(a){b}elseif(c){d}elseif(e){f}else{g} is translated in
     * if(a){b}else{if(c){d}else{if(e){f}{g}}} etc.
     *)
    | A.If (tok, cond1, stmt1, elseifs, elseopt) ->
        (* there cannot be any break statements inside an if so 
         * that should not cause a problem *)
        let rec (aux : (tok * A.expr paren * A.stmt) list ->
                   (tok * A.stmt) option -> B.stmt list)
                   = fun elseifs elseopt ->
        (match elseifs, elseopt with
        | [], None -> []
            
        | (_, condA, stmtA)::tl, elseopt ->
            let instrs_cond, expr_cond = condA |> Ast.unparen 
              |> linearize_expr in
            let stmts_cond = instrs_to_stmts instrs_cond in
            let stmtsB = aux_stmt stmtA in
            stmts_cond ++ [ B.If(expr_cond, B.Block stmtsB, 
                                 stmt_of_stmts_list (aux tl elseopt)) ]

        | [], Some (_, stmtA) -> aux_stmt stmtA

        ) in
        (* the if case is not any different than all the other
         * elseif cases; it just appears first *)
        aux ((tok, cond1, stmt1)::elseifs) elseopt

    (* this is very similar to the A.If case;
     * it seems too complicated to do just one case for both though 
     * less: IfColon is an old PHP feature we don't have to support.
     *  could remove this whole case.
     *)
    | A.IfColon (tok1, cond1, tok2, stmt1, elseifs, elseopt, _, _) ->
        let rec (aux : A.new_elseif list ->
                   A.new_else option -> B.stmt list)
                   = fun elseifs elseopt ->
        (match elseifs, elseopt with
        | [], None -> []
            
        | (_, condA, _, stmtsA)::tl, elseopt ->
            let instrs_cond, expr_cond = condA |> Ast.unparen 
              |> linearize_expr in
            let stmts_cond = instrs_to_stmts instrs_cond in
            let stmtsB = stmtsA |>  List.map aux_stmt_and_def 
              |> List.flatten in
            stmts_cond ++ [ B.If(expr_cond, B.Block stmtsB, 
                                 stmt_of_stmts_list (aux tl elseopt)) ]

        | [], Some (_, _, stmtsA) -> 
            stmtsA |> List.map aux_stmt_and_def |> List.flatten

        ) in
        (* the if case is not any different than all the other
         * elseif cases; it just appears first *)
        aux ((tok1, cond1, tok2, stmt1)::elseifs) elseopt

    (* This is a hard one, even if it does not look like it;
     * here is a way to translate it that should work:
     * switch(expr){
     *   case expr_a :
     *     do_a;
     *   case expr_b :
     *     do_b;
     *   ...
     *   default :
     *     do_default
     *   ...
     * }  
     *     is translated as:
     * $expr = expr;
     * $cond_while = true;
     * while($cond_while){
     *   $cond_while = false; // only goes through once
     *   $case = 0;
     *   if($expr == expr_a){ $case=1; }
     *   else if($expr == expr_b){ $case=2; }
     *   // b should not be evaluated if we are in the case a
     *   // (only relevant if b has side effects)
     *   ...
     *   // nothing here for default
     *   ...
     *   $cond = ($case == 1) || $cond;
     *   if($cond){ do_a; }
     *   $cond = ($case == 2) || $cond;
     *   if($cond){ do_b; }
     *   ...
     *   $cond = ($case == 0) || $cond; // 0 is for default
     *   if($cond){ do_d; }
     *   ...
     * }
     * The enclosing while($cond) is to make sure everything
     * works fine if there is a break instruction in the middle; we
     * could have done it with while(true){... ; break;} but our way
     * ensures it also works with continue instructions.
     * The two passes are necessary to ensure default behaves correctly:
     * expr_a, expr_b, are evaluated in order until one applies; if none
     * applies, default applies; if default applies all the expr_a,
     * expr_b, etc. have to be evaluated, even
     * if they are after the default. default only applies if no other
     * case applies, even if default is not at the end.
     *)  
    | A.Switch(_, exprA, switch_cases) ->
        let instrs_expr, exprB = exprA |> Ast.unparen
          |> linearize_expr in
        let stmts_expr = instrs_to_stmts instrs_expr in
        let expr_vv = fresh_vvar ()
        and cond_while_vv = fresh_vvar ()
        and case_vv = fresh_vvar()
        and cond_vv = fresh_vvar()
        and i = ref 0 in (* counter of the cases *)
        let rec (aux : A.case list -> (B.stmt list)*(B.stmt list)) = function
          (* this generates the two series of if statements
           * if_stmt is what is to be included in the else branch
           * of the first serie *)
        | [] -> [], []
        | Case(_, eA, _, stmtA)::tl ->
            let instrs_eB, eB = linearize_expr eA in
            let stmts_eB = instrs_to_stmts instrs_eB
            and stmtB = stmtA |> (List.map aux_stmt_and_def)
                |> List.flatten |> stmt_of_stmts_list in
            let index = !i in i := !i+1;
            let l1, l2 = aux tl in
            ( stmts_eB ++ 
              [ B.If(mkt(B.Binary(mkt(B.Lv(expr_vv)), A.Logical A.Eq, eB)),
                     B.Instr(B.Assign(case_vv, B.AssignEq, 
                                      expr_of_int index)),
                     stmt_of_stmts_list l1) ]
            ), (
              B.Instr(B.Assign(cond_vv, B.AssignEq, 
                               mkt(B.Binary(mkt(B.Binary(mkt(B.Lv case_vv),
                                                         A.Logical A.Eq,
                                                         expr_of_int index)),
                                            A.Arith A.Or,
                                            mkt(B.Lv cond_vv))))) ::
              B.If(mkt(B.Lv cond_vv), stmtB, B.EmptyStmt) ::
              l2
            )
        | Default(_, _, stmtA)::tl ->            
            let stmtB = stmtA |> (List.map aux_stmt_and_def)
                |> List.flatten |> stmt_of_stmts_list in
            let l1, l2 = aux tl in
            l1, (
              B.Instr(B.Assign(cond_vv, B.AssignEq, 
                               mkt(B.Binary(mkt(B.Binary(mkt(B.Lv case_vv),
                                                         A.Logical A.Eq,
                                                         expr_of_int 0)),
                                            A.Arith A.Or,
                                            mkt(B.Lv cond_vv))))) ::
              B.If(mkt(B.Lv cond_vv), stmtB, B.EmptyStmt) ::
              l2
            ) 
        in
        let cases = match switch_cases with
          | CaseList(_, _, c, _) | CaseColonList( _, _, c, _, _) -> c in
        let l1, l2 = aux cases in
        stmts_expr ++ 
        [ B.Instr(B.Assign(expr_vv, B.AssignEq, exprB))] ++
        [ B.Instr(B.Assign(cond_while_vv, B.AssignEq, expr_true())) ] ++
        [ B.While(mkt(B.Lv(cond_while_vv)), B.Block(
            [ B.Instr(B.Assign(cond_while_vv, B.AssignEq,expr_false())) ] ++
            [ B.Instr(B.Assign(case_vv, B.AssignEq, expr_of_int 0)) ] ++
            l1 ++ l2))
        ]

    | A.Break(_, None, _) -> [ B.Break None ]
    | A.Break(_, Some exprA, _) ->
        let instrs, expr = linearize_expr exprA in
        (instrs_to_stmts instrs) ++ [ B.Break (Some expr) ]
      
    | A.EmptyStmt _ -> [ ] (* or [ B.EmptyStmt ] ? *)

    | A.Continue(_, None, _) -> [ B.Continue None ]
    | A.Continue(_, Some exprA, _) ->
        let instrs, expr = linearize_expr exprA in
        (instrs_to_stmts instrs) ++ [ B.Continue (Some expr) ]

    | A.Throw(_, exprA, _) ->
        let instrs, expr = linearize_expr exprA in
        (instrs_to_stmts instrs) ++ [ B.Throw expr ]

    | A.Use _ ->
         failwith "This Use should be lifted by another simplification phase"

    |
        (Declare (_, _, _)|Unset (_, _, _)|InlineHtml _|
            StaticVars (_, _, _)|Globals (_, _, _)|Try (_, _, _, _))
      -> raise Todo

  and aux_colon_stmt colon = 
    match colon with
    | A.SingleStmt st -> aux_stmt st
    | A.ColonStmt(_, stl, _, _) -> stl |> List.map aux_stmt_and_def
      |> List.flatten
  and aux_stmt_and_def x = 
    match x with
    | A.Stmt st -> aux_stmt st
    | (InterfaceDefNested _|ClassDefNested _|FuncDefNested _) ->
        raise Todo
  in
  let stmts = aux_stmt st in
  stmts


let (linearize_body: Ast_php.stmt_and_def list -> Pil.stmt list) = fun xs ->
  xs +> List.map (fun x ->
    match x with
    | A.Stmt st -> linearize_stmt st
    | (InterfaceDefNested _|ClassDefNested _|FuncDefNested _) ->
        raise Todo
  ) +> List.flatten


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)


let pil_of_program ast = 
  raise Todo

(*e: pil_build.ml *)
