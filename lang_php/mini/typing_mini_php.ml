(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Ast_mini_php

module V = Visitor_mini_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * PHP does not really allow higher order functions, so we dont have to
 * infer function type for variable
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* 
 * http://en.wikipedia.org/wiki/Type_inference
 *)

type typevar = string

and contrainte = 
  | Eq of phptypebis * phptypebis

  | In of typevar * phptype

and constraints = contrainte list
 (* with sexp *)

type subst = (typevar * phptypebis) list

exception CantUnify of phptypebis * phptypebis

(*****************************************************************************)
(* Helpers, visitor and sexp *)
(*****************************************************************************)

(* visitor *)
let v_unit x = ()
let v_string (s:string) = ()
let v_ref aref x = () (* dont go into ref *)
let v_option v_of_a v = 
  match v with
  | None -> ()
  | Some x -> v_of_a x
let v_list of_a xs = 
  List.iter of_a xs


(* sexp *)
let rec sexp_of_typevar v = Conv.sexp_of_string v
and sexp_of_contrainte =
  function
  | In ((v1, v2)) ->
      let v1 = sexp_of_typevar v1
      and v2 = Sexp_mini_php.sexp_of_phptype v2
      in Sexp.List [ Sexp.Atom "In"; v1; v2 ]
  | Eq ((v1, v2)) ->
      let v1 = Sexp_mini_php.sexp_of_phptypebis v1
      and v2 = Sexp_mini_php.sexp_of_phptypebis v2
      in Sexp.List [ Sexp.Atom "Eq"; v1; v2 ]
and sexp_of_constraints v = Conv.sexp_of_list sexp_of_contrainte v

let string_of_constraints xs = 
  let sexp = sexp_of_constraints xs in
  let s = Sexp.to_string_hum sexp in
  s

let sexp_of_subst = 
  Sexp_common.sexp_of_assoc 
    (Conv.sexp_of_string) 
    (Sexp_mini_php.sexp_of_phptypebis)
let string_of_subst x = 
  Sexp.to_string_hum (sexp_of_subst x)

(*****************************************************************************)
(* *)
(*****************************************************************************)

let cnt = ref 0
let new_typevar prefix = 
  incr cnt;
  TypeVar (spf "%s_%d" prefix !cnt)

let str_return = "__return"
let typevar_return = 
  TypeVar str_return



(*****************************************************************************)
(* Step1: annotating AST with typevars *)
(*****************************************************************************)

let annotate_initial_with_typevars env_params stmts =
  let env = ref env_params in
  let hooks = { V.default_visitor with
    V.kexpr = (fun (k,vx) e ->
      let info = snd e in

      let t = 
        match fst e with
        | Var s ->
            (try List.assoc s !env
            with Not_found ->
              let newt = new_typevar s in
              Common.push2 (s, newt) env;
              newt
            )
        | _ -> new_typevar "T"
      in
      info.t <- [t];
      k e
    );
  } 
  in
  let visitor = V.mk_visitor hooks in
  stmts +> List.iter visitor.V.vstmt;
  ()


(*****************************************************************************)
(* Step2: Generating constraints *)
(*****************************************************************************)

let t e = 
  let (_, info) = e in
  match info.t with
  | [TypeVar typevar] -> TypeVar typevar
  | _ -> raise Impossible

let generate_equations stmts = 
  let eqs = ref [] in
  let addeq (t1, t2) = Common.push2 (Eq(t1,t2)) eqs; () in
 
  let rec v_stmt st = 
    match st with
    | ExprStmt v1 -> v_expr v1
    | Echo v1 -> v_expr v1

    | If ((v1, v2, v3)) ->
        v_expr v1;
        v_stmt v2;
        v_option v_stmt v3;

        addeq (t v1, TBool)

    | While ((v1, v2)) -> 
        v_expr v1;
        v_stmt v2;
        
        addeq (t v1, TBool)
    | Block v1 -> 
        v_list v_stmt v1
    | Return v1 -> 
        v_option v_expr v1;
        (match v1 with
        | None ->
            addeq (typevar_return, TUnit)
        | Some v1 ->
            addeq (t v1, typevar_return)
        )

  and v_expr e =
    let (ebis, info) = e in
    match ebis with
    | Bool v1 -> 
        addeq ((t e, TBool))
    | Number v1 -> 
        addeq ((t e, TNum))
    | String v1 -> 
        addeq ((t e, TString))
    | Null -> 
        addeq ((t e, TNull))
    | Var v1 -> 
        ()

    | ArrayAccess ((v1, v2)) -> 
        v_expr v1;
        v_expr v2;

        addeq (t v2, TString);
        let nvar = new_typevar "TA" in
        addeq (t v1, TArray([nvar]))
        
    | Assign ((v1, v2)) -> 
        v_expr v1;
        v_expr v2;

        addeq (t v1, t v2)
    | Binary ((v1, v2, v3)) ->
        v_expr v1;
        (* Ast_php.v_binaryOp v2 *)
        v_expr v3;

        (* addeq (t v1, t v3) ? *)
        (match v2 with
        | _ -> ()
        )
    | Funcall ((v1, v2)) ->
        v_string v1;
        v_list v_expr v2;

        raise Todo
                                                       
  in
  stmts +> List.iter v_stmt;
  !eqs

(*****************************************************************************)
(* Step3: solving the constraints *)
(*****************************************************************************)

let rec visit_tvar_phptypebis fvar tbis = 
  match tbis with
  | TBool -> TBool
  | TNum -> TNum
  | TString -> TString
  | TUnit -> TUnit
  | TArray v1 -> let v1 = visit_tvar_phptype fvar v1 in 
                 TArray v1
  | TNull -> TNull
  | TVariant -> TVariant
  | TUnknown -> TUnknown
  | TypeVar v1 -> 
      fvar v1
  | TRecord _ -> raise Todo
  | THash _ -> raise Todo

and visit_tvar_phptype fvar xs = 
  List.map (visit_tvar_phptypebis fvar) xs



let occur_check svar t = 
  t +> visit_tvar_phptypebis (fun s ->
    if s = svar
    then failwith ("occur check of :" ^ s)
    else TypeVar s
  ) +> ignore

let occur_check_subst svar substs = 
  substs +> List.iter (fun (svar2, t2) ->
    if svar = svar2 
    then failwith ("occur check in subst of :" ^ svar);
    occur_check svar t2
  )

let apply_substs_tbis substs tbis =
  tbis +> visit_tvar_phptypebis (fun var ->
    try List.assoc var substs 
    with Not_found -> TypeVar var
  )


(* assume that subst1 type still contains typevar mentionned in 
 * subst, and the typevar of subst1 is never in subst
 *)
let compose_subst subst1 subst = 
  let (s, t) = subst1 in
  occur_check_subst s subst;
  
  let t' = apply_substs_tbis subst t in
  (s, t')::subst
  


let apply_subst_eqs subst1 xs =
  let (s, t) = subst1 in
  xs +> List.map (function
  | Eq (t1, t2) ->
      let fvar svar = 
        if s = svar 
        then t
        else TypeVar svar (* bugfix: svar!!! not s, of course *)
      in
      Eq (visit_tvar_phptypebis fvar t1, 
         visit_tvar_phptypebis fvar t2)
  | In _ -> raise Impossible
  )

(* return a set of new equations and a subst *)
let rec unify_one_step t1 t2 = 
  match t1, t2 with
  | TypeVar s1, TypeVar s2 ->
      if s1 = s2
      then [], None
      else 
        let subst1 = (s1, TypeVar s2) in
        [], Some subst1
  | TypeVar svar, t
  | t, TypeVar svar
    ->
      occur_check svar t;
      let subst1 = (svar, t) in
      [], Some subst1

  | TBool, TBool
  | TNum, TNum
  | TString, TString
  | TUnit, TUnit

  | TNull, TNull
      -> [], None

  | TArray t1, TArray t2 ->
      (match t1, t2 with
      | [t1], [t2] ->
          unify_one_step t1 t2
      | _ -> raise Impossible
      )

  | TVariant, _
  | _, TVariant
    
  | TUnknown, _
  | _, TUnknown
      -> raise Impossible

  | _ -> raise (CantUnify (t1, t2))


(* cf p327 of TAPL *)
let rec solve_equations xs = 
  match xs with
  | [] -> [] (* empty subst *)
  | x::xs ->
      (match x with
      | Eq (t1, t2) ->
          let new_eqs, subst_maybe = unify_one_step t1 t2 in
          let xs' = 
            match subst_maybe with
            | None -> xs
            | Some subst ->
                apply_subst_eqs subst xs
          in
          let xs'' = new_eqs ++ xs' in
          let subst = solve_equations xs'' in
          (match subst_maybe with
          | None -> subst
          | Some subst1 -> compose_subst subst1 subst
          )
                
          

      | In _ -> 
          raise Impossible
      )

(*****************************************************************************)
(* *)
(*****************************************************************************)


let typing ast = 
  match ast with
  | FuncDef (s, params, stmts) ->
      let env = params +> List.map (fun (s, eopt) ->
        (s, new_typevar s)
      )
      in
      (* TODO: generate constraints for default value too *)
      annotate_initial_with_typevars env stmts;
      
      pr2 "Program:";
      let s = Sexp_mini_php.string_of_program [ast] in
      pr2 s;

      pr2 "Equations:";
      let eqs = generate_equations stmts in
      let s = string_of_constraints eqs in
      pr2 s;

      pr2 "Solving:";

      (try 
        let subst = solve_equations eqs in
        let s = string_of_subst subst in
        pr2 s;

        pr2 "Type of function:";
        env +> List.iter (fun (s, tvar) ->
          (match tvar with
          | TypeVar tvar ->
              let typ = List.assoc tvar subst in
              pr2_no_nl (spf "%s:%s " s (Sexp_mini_php.string_of_phptypebis typ));
          | _ -> raise Impossible
          )
        );
        let typreturn = List.assoc str_return subst in
        pr2 (spf "-> %s" (Sexp_mini_php.string_of_phptypebis typreturn))
        
      with 
       e -> raise e (* TODO *)
      )
      

      
  | _ -> failwith "typing only function"
