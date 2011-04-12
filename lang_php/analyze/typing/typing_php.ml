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

open Ast_php

module Flag = Flag_analyze_php
module Ast = Ast_php

module V = Visitor_php
module T = Type_php
module N = Namespace_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * PHP history: 
 * 
 * What type system ? Want a Poly of 'a ?
 * When see $x = $y and have no info on y, still want to remember
 * that the type of x and y must unify. Could do that either:
 *  - indirectly by having them share the same type variable
 *  - having environment reference, so that the type of x leads to y
 * 
 * See:
 * ./cours/insa/compil-4eme-annee-bis/tp4/infereur.ml
 * ./cours/insa/compil-4eme-annee-bis/tp4/infereur2.ml
 * ./cours/insa/prolog-4eme-annee/tpn.pr
 * ./teaching/insa/compil-4info-2/pad/yacc/inference.ml
 * ./teaching/insa/compil-4info-2/pad/yacc/inference3.ml
 * ./teaching/insa/compil-4info-2/cahier/tp4/tp4-sf/tp01/tp4.ml
 * 
 * As opposed to the type inferer I did in the past, a difference here
 * is that we also want to annotate each expr node with its type.
 * 
 * Another difference is that we dont want necessaraly type error when 
 * stuff does not unify; we may want instead to generate a Union type,
 * which itself leads to a more complicated unifier. It also change
 * the way we have to handle the environment. Indeed usually when
 * you see a x, you look in the env its type, and then you unify
 * it with other stuff, and apply the subst to the env, and you hope
 * that it will by side effect modify the type of x. But what if 
 * the type of X is null, and that we unify it with an int, then
 * we want to change the type of X to null|int but the info that
 * null was binded to X is lost. A trick may be to use type variable hole,
 * so that at first X is not binded to null but to Union(null, A),
 * and when we unify it with int, then we generate a Union(null,int, B)
 * and continue like that. In the end I think it is probably
 * simpler to do type inference via the contraints technique.
 * 
 * A final difference
 * is the need to incorporate interprocedural analysis in it (because
 * when we see a function call, we want to know the type of this function).
 * 
 * 
 * 
 * Choices:
 *   - infer via constraints
 *   - infer via ast visiting and passing environment
 *     and infer by also passing contextual info (like want_a_bool when
 *     analyzing a If, but normally type inference does exactly that)
 * 
 *   - infer using globals, less need to thread, but still need to take
 *     care when found a substitution
 *   - unify via subst (and take care of invariant, must subst in subst)
 *   - unify a la Prolog, with shared env
 *
 * 
 * 
 * History: 
 *  - Done a first type checker in 2002, cf typing-semantic/, but
 *    was assuming that have all type info, and so was assuming had called
 *    cpp and everything was right.
 *  - Wrote this file, in 2006?, as we added pattern matching on type 
 *    in coccinelle. Partial type annotater.
 *  - Julia extended it in 2008? to have localvar/notlocalvar and 
 *    test/notest information, again used by coccinelle.
 *  - I extended it in Fall 2008 to have more type information for the 
 *    global analysis. I also added some optimisations to process
 *    included code faster.
 *  - Port for handling php in 2009
 * 
 *  
 * (old) Design choices. Can either do:
 *  - a kind of inferer
 *     - can first do a simple inferer, that just pass context
 *     - then a real inferer, managing partial info.
 *    type context = fullType option
 *
 *  - extract the information from the .h files
 *    (so no inference at all needed)
 * 
 * Difference with julia's code in parsing_cocci/type_infer.ml:
 *  - She handles just the variable namespace. She does not type
 *    field access or enum or macros. This is because cocci programs are
 *     usually simple and have no structure definition or macro definitions
 *     that we need to type anyway.
 *  - She does more propagation.
 *  - She does not have to handle the typedef isomorphism which force me 
 *    to use those typedef_fix and type_unfold_one_step
 *  - She does not handle I think the function pointer C isomorphism.
 * 
 *  - She has a cleaner type_cocci without any info. In my case 
 *    I need to do those ugly al_type, or generate fake infos.
 *  - She has more compact code. Perhaps because she does not have to 
 *    handle the extra exp_info that she added on me :) So I need those
 *    do_with_type, make_info_xxx, etc.
 * 
 * Note: if need to debug this annotater, use -show_trace_profile, it can
 * help. You can also set the typedef_debug flag below.
 * 
 * 
 * 
 * todo: expression contain types, and statements,   which in turn can contain
 * expression, so need recurse. Need define an annote_statement and 
 * annotate_type.
 * 
 * todo: how deal with typedef isomorphisms ? How store them in Ast_c ?
 * store all posible variations in ast_c ? a list of type instead of just
 * the type ?
 * 
 * todo: how to handle multiple possible definitions for entities like
 * struct or typedefs ? Because of ifdef, we should store list of 
 * possibilities sometimes.
 * 
 * todo: define a new type ? like type_cocci ? where have a bool ?
 * 
 * semi: How handle scope ? When search for type of field, we return 
 * a type, but this type makes sense only in a certain scope.
 * We could add a tag to each typedef, structUnionName to differentiate 
 * them and also associate in ast_c to the type the scope
 * of this type, the env that were used to define this type.
 * 
 * todo: handle better the search in previous env, the env'. Cf the 
 * termination problem in typedef_fix when I was searching in the same
 * env.
 * 
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_typing


(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* There is 2 concepts involved in type inference: the environment, and
 * substitutions. One is for program variable -> type, the other is for 
 * type variable to type.
 * 
 * Invariant: when we found a substitution for a type variable, we
 * must absolutely apply it everywhere, in the environment, and in
 * other substitutations we may have to compose with. Otherwise, 
 * the env would have some "dangling" typevars in certain types
 * which will never be reduced, or some substitutions would still speak
 * about some typevars that have long been reduced. Moreover
 * we will have to compose substitutions and in that case we dont
 * want to unify substitutions by having one subst saying 'a -> ent
 * and the other 'a -> bool. Once we have found the value of 'a,
 * it should disappear everywhere.
 * 
 * Invariant: on ne peut pas avoir 2 appels infere avec le meme env, 
 * car 1 et 2 peuvent retourner comme subst x -> H et x -> V => 
 * ca oblige apres a faire de l'unification d'unificateur.
 * Si on a trouver que x valait H, alors la prochaine fois que 
 * quelqu'un veut la valeur de x, alors il faut que ca soit H, 
 * et pas un vieux binding de x.
 * Il faut threader l'environnement ou le gerer en global.
 * Donc principe de precaution !! des que unif, des vars de types sont
 * instancies donc il faut changer ca partout !! 
 * dans l'env ET aussi dans les types inferes jusque la.
 *
 * We will manage the environment as a global mutable hashtbl (simpler), 
 * so no need to thread it across the expr. Note that Hashtbl.add
 * can remember the old value, so we can use this functionnality to 
 * implement a scoping mechanism.
 * 
 * TODO for params easy, for local, have to remember which locals were
 * created and remove them as a hook at the end of the function analysis.
 * 
 * TODO later may need a more complex env, because have multiple namespaces
 * 
 *)
type environment = (N.dnameS, T.phptype) Hashtbl.t

let cnt = ref 0
let new_typevar str = 
  incr cnt; 
  T.TypeVar (str ^ "_" ^ (i_to_s !cnt))

type typevar = string

type substitution = (typevar, T.phptype) Hashtbl.t

let rec (subst: substitution -> T.phptype -> T.phptype) = fun subst t ->
  (* fold *)
  raise Todo

(* invariant: one is more specialize than the other.
 * It is better to have 'a -> ent than 'a -> 'b; 'b -> ent.
 * We suppose that we have in arg2 some more specialised subst than in arg1,
 * otherwise would have to unify substitutation ...
 *)
let compose_subst s1 s2 = 
  raise Todo

let occurs typevar t = 
  raise Todo

exception CanNotUnify of T.phptype * T.phptype

let env_subst subst env =
  raise Todo

(*****************************************************************************)
(* Type ordering, see typing_trivial_php.ml *)
(*****************************************************************************)

(*****************************************************************************)
(* Unify *)
(*****************************************************************************)

(* TODO: use quicheck to test my unifier ? *)

let rec unify t1 t2 = 
  raise Todo
(*
  match t1, t2 with
  | T.Unknown, _ -> t2

  | T.Bool, T.Bool -> t1
  | T.Bool, _ -> 
      T.Top
  | T.Int, T.Int -> t1
  | T.Int, _ ->
      T.Top
  | T.Float, T.Float -> t1
  | T.Float, _ ->
      T.Top
  | T.String, T.String -> t1
  | T.String, _ ->
      T.Top
  | T.Unit, T.Unit -> t1
  | T.Unit, _ ->
      T.Top

  | T.Array t1, T.Array t2 -> 
      T.Array (unify t1 t2)
  | T.Array t1, _ ->
      T.Top
  | T.Hash t1, T.Hash t2 -> 
      T.Hash (unify t1 t2)
  | T.Hash t1, _ ->
      T.Top 
  | T.Record xs1, T.Record xs2 -> 
      T.Record (xs1 ++ xs2)
  | T.Record xs1, _ ->
      T.Top 

  | T.Object sopt1, T.Object sopt2 -> 
      (* TODO *)
      T.Object sopt2
  | T.Object _, _ ->
      T.Top

  | T.Null, T.Null ->  t1
  | T.Null, _ -> 
      T.Top

  | T.Union xs1, T.Union xs2 -> 
      T.Top

  | T.Union _, _ ->
      T.Top

  | T.Top, _ -> T.Top 
*)


(*****************************************************************************)
(* Basic typing *)
(*****************************************************************************)

let type_of_scalar env scalar = 
  raise Todo
(*
  match scalar with 
  | C cst ->
      (match cst with
      | Int _ -> T.Int
      | Double _ -> T.Float
      | String _ -> T.String
          
      | CName name -> 
          let s = Ast.name name in
          (* move in a 'let type_name_builtins = [...]' ? *)
          (match s with
          | "true" -> T.Bool
          | "false" -> T.Bool
          | "null" -> T.Null
          | _ -> T.Unknown
          )
      | PreProcess directive -> T.String
      )
  | ClassConstant cl ->
      (* TODO *)
      T.Unknown
  | Guil _ -> T.String
  | HereDoc _ -> T.String
*)

(*****************************************************************************)
(* Constraint solver *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* 
 * TODO: take full program ? but what about scope ? 
 * have to process the require ? or do global ?
 * or do semi global, that is process file by file ?
 *)


(* shortcut *)
let t e = Ast.get_type e


(* can modify the env ? and unify sutff ? *)
let type_of_binary env t1 op t2 = 
  raise Todo
(*
  match op with
  | BinaryConcat ->
      (match t1, t2 with
      | T.String, T.String -> T.String
      (* TODO, unify sone stuff ? *)
      | _ -> T.String
      )
  | Arith op ->
      (match t1, t2 with
      | T.Int, T.Int -> T.Int
      (* TODO, unify sone stuff ? *)
      | _ -> T.Int
      )
  | Logical op ->
      (match t1, t2 with
      | _ -> T.Bool
      )
*)

(* assume the visitor have called the annotater on its subcomponents *)
let rec type_of_expr env e = 
  match e with
  | Lv var -> 

      (match untype var with
      | Var (dname, scope) ->
          let namevar = N.dnameS_of_dname dname in

          (match Common.hfind_option namevar !env with
          | Some t -> t
          | None ->
              raise Todo
                (*
                  Hashtbl.add !env namevar T.Unknown;
                  T.Unknown
                *)
          )

      | VArrayAccess (var2, expr_bracket) ->
          (match untype var2, Ast.unbracket expr_bracket with

          | (Var (dname, scope), 
            Some (Sc (C (Ast.String (s, info))), t)) ->
              
              let namevar = N.dnameS_of_dname dname in
              (match Common.hfind_option namevar !env with
              | Some t -> 
                  raise Todo
                    (*
                      let t' = unify t 
                      
                      (T.Record [s, T.Unknown])
                      
                      in
                      Hashtbl.replace !env namevar t';
                      
                    (* TODO: find type in unified type, but
                      * probably Unknown for now
                    *)
                      T.Unknown
                    *)
              | None -> [T.Unknown]
              )

          | _ -> [T.Unknown]
          )
      | _ -> [T.Unknown]
      )



  | Sc scalar -> 
      type_of_scalar env scalar

  | Assign ((v1, v2, v3)) ->
      (* unify !! *)
      raise Todo
(*
      let t3 = t v3 in

      (match v1 with
      | Variable (basevar_and_fun, objs) ->

          (match basevar_and_fun with
          | BaseVar base ->
              let (qu_opt, (indirect, refvar)) = base in
              (match refvar with
              | Var (dname, scope) ->
                  let namevar = N.dnameS_of_dname dname in
                  (match Common.hfind_option namevar !env with
                  | Some t -> 
                      let t' = unify t t3 in
                      Hashtbl.replace !env namevar t';
                      t3
                      
                  | None ->
                      raise Todo
                        (*
                      Hashtbl.add !env namevar t3;
                      t3
                        *)
                  )

              | VArrayAccess (refvar2, expr_bracket) ->
                  (match refvar2, Ast.unbracket expr_bracket with
                  | (Var (dname, scope), 
                    Some (Sc (C (Ast.String (s, info))), t)) ->

                      let namevar = N.dnameS_of_dname dname in
                      (match Common.hfind_option namevar !env with
                      | Some t -> 
                          let t' = unify t 
                            (T.Record [s, t3])

                          in
                          Hashtbl.replace !env namevar t';

                          (* TODO: find type in unified type, but
                           * probably Unknown for now
                           *)
                          t3

                      | None -> t3
                      )
                  | _ -> t3
                  )
              | _ -> t3
              )
          | FunCall _ -> t3
          )
      )
*)

  | AssignRef ((v1, v2, v3, v4)) ->
      [T.Unknown]
  | AssignNew ((v1, v2, v3, v4, v5, v6)) ->
      [T.Unknown]
  | AssignOp ((v1, v2, v3)) ->
      [T.Unknown]
  | Postfix ((v1, v2)) ->
      [T.Unknown]
  | Infix ((v1, v2)) ->
      [T.Unknown]
  | Binary ((e1, op, e3)) ->
      raise Todo
       (*
      let op = unwrap op in
      type_of_binary env (t e1) op (t e3)
       *)

  | Unary ((v1, v2)) ->
      [T.Unknown]

  | CondExpr ((v1, v2, v3, v4, v5)) ->
      [T.Unknown]

  | AssignList ((v1, v2, v3, v4)) ->
      [T.Unknown]
  | ConsArray ((v1, v2)) ->
      [T.Unknown]
  | New ((v1, v2, v3)) ->
      [T.Unknown]
  | Clone ((v1, v2)) ->
      [T.Unknown]
  | InstanceOf ((v1, v2, v3)) ->
      [T.Unknown]
  | Cast ((v1, v2)) ->
      [T.Unknown]
  | CastUnset ((v1, v2)) ->
      [T.Unknown]
  | Exit ((v1, v2)) ->
      [T.Unknown]
  | At ((v1, v2)) ->
      [T.Unknown]
  | Print ((v1, v2)) ->
      [T.Unknown]
  | BackQuote ((v1, v2, v3)) ->
      [T.Unknown]
  | Include ((v1, v2)) ->
      [T.Unknown]
  | IncludeOnce ((v1, v2)) ->
      [T.Unknown]
  | Require ((v1, v2)) ->
      [T.Unknown]

  | RequireOnce ((v1, v2)) ->
      [T.Unknown]
  | Empty ((v1, v2)) ->
      [T.Unknown]
  | Isset ((v1, v2)) ->
      [T.Unknown]
  | Eval ((v1, v2)) ->
      [T.Unknown]
  | ParenExpr v1 -> 
      [T.Unknown]

  | (SgrepExprDots _|XhpHtml _|Lambda _ |Yield _ | YieldBreak _) -> 
      raise Todo


let return_hack = "__return"




let annotate_with_fresh_typevars_every_expr envref def = 

  let hooks = { V.default_visitor with
    V.kexpr = (fun (k,vx) e ->
     
      let t = new_typevar "t" in
      Ast.set_type e [t];
      k e
    );
  }
  in
  (V.mk_visitor hooks) (Body def.f_body)


(* envref will contain the binding for globals *)
let annotate_toplevel envref ast =
  
  (match ast with
  | FuncDef def -> 

      (* compute environment *)

      let params = Ast.uncomma (Ast.unparen def.f_params) in
      params +> List.iter (fun param ->
        let s = Ast.dname param.p_name in
        let name = N.dnameS_of_dname param.p_name in
        
        Hashtbl.add !envref name [new_typevar s]; 
      );
      Hashtbl.add !envref (N.DNameS return_hack) [new_typevar return_hack];

      (* TODO if default val, scalar, compute type and assign *)
      
      annotate_with_fresh_typevars_every_expr envref def;

      (* 
       * let eqs = generate_type_equations def in
       * let subst = solve_eqs eqs in
       * reannotate_typevars_every_expr subst def;
       * params +> List.iter (fun param -> ... Hashtbl.remove );
       *
       * TODO and now look again in env to find the final type for the
       * params
       *
       * let typ_params = 
       * params +> List.map (fun param ->
       * let name = N.dnameS_of_dname param.p_name in 
       * Some (Hashtbl.find !envref name)
       * )
       * in
       * def.f_type := T.Function (T.Unknown, 
       * typ_params);
       *)

      ()

  | StmtList v1 -> 
      pr2 "Todo"
  | ClassDef v1 -> 
      pr2 "Todo"
  | InterfaceDef v1 -> 
      pr2 "Todo"
  | Halt ((v1, v2, v3)) -> 
      pr2 "Todo"

        
  | NotParsedCorrectly xs -> ()
  | FinalDef eof -> ()
  );

  ast

