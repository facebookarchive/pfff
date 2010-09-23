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

module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * history: repeat what iain wanted for his Strict mode that I first
 * coded in hphpi
 * 
 * related work:
 *  - miyamide
 *  - PHP-sat at http://strategoxt.org/PHP/PhpSat
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* 
 * Namespaces of php ?? Same string can represent a class and a function ?
 * At least have the T_STRING (regular ident) vs T_VARIABLE (dollar idents)
 * difference.
 * 
 * Because have nested scope, have nested list, hence the list list.
 *
 * opti? use a hash to accelerate ? 
 *)
type entity = 
  | AFunc of name (* for error loc *) * parameter list * 
      bool (* has call to func_args or stuff like that *)
  | AClass of name (* for error loc *) * method_def list * 
      extend option

type environment = (name * entity) list list 


(* ------------------------------------------------------------ *)
(* use a ref because we may want to modify it *)
let (initial_env: environment ref) = ref [
  []
]

(* ------------------------------------------------------------ *)
let rec lookup_env2 s env = 
  match env with 
  | [] -> raise Not_found
  | []::zs -> lookup_env2 s zs
  | ((a,b)::xs)::zs -> 
      if Ast_php.name a = s 
      (* TODO may need to return env at this place ??? xs::zs 
       * For instance when look for a parent class in scope, and 
       * then search again the parent of this parent, should start
       * from the scope at the parent place.
       * 
       * But in fact it should be static, because php is dynamic so 
       * should look at parent chain of things at run-time!
       * But maybe in practice for most of the codebase, people
       * dont define complex recursive class or dynamic hierarchies so
       * doing it statically in a naive way is good enough.
       *)
      then b 
      else lookup_env2 s (xs::zs)
let lookup_env a b = 
  Common.profile_code "TAC.lookup_env" (fun () -> lookup_env2  a b)


let lookup_env_opt a b = 
  Common.optionise (fun () -> lookup_env a b)


(*****************************************************************************)
(* Helpers, part 1 *)
(*****************************************************************************)


(*****************************************************************************)
(* Typing rules *)
(*****************************************************************************)
(* cf also type_php.ml *)

let no_check_when_contain = [
  "func_num_args";
]

let check_args_vs_params ((callname:name), args) ((defname:name), params) =

  let info = Ast_php.info_of_name callname in
  let pinfo = parse_info_of_info info in
  let args_error = pinfo, defname in

  let rec aux args params = 
    match args, params with
    | [], [] -> ()
    | [], y::ys ->
        if y.p_default = None 
        then E.fatal (E.NotEnoughArguments args_error)
        else aux [] ys
    | x::xs, [] ->
        E.fatal (E.TooManyArguments args_error)
    | x::xs, y::ys ->
        aux xs ys
        
  in
  aux args params


(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* opti: cache ? use hash ? *)
let _scoped_env = ref !initial_env

(* memoise unnanoted var, to avoid too much warning messages *)
let _notyped_var = ref (Hashtbl.create 100)

(* TODO use generic implem of Common ? *)
let new_scope() = _scoped_env := []::!_scoped_env 
let del_scope() = _scoped_env := List.tl !_scoped_env

let do_in_new_scope f = 
  begin
    new_scope();
    let res = f() in
    del_scope();
    res
  end

let add_in_scope namedef =
  let (current, older) = Common.uncons !_scoped_env in
  _scoped_env := (namedef::current)::older

(* ------------------------------------------------------------ *)
let add_binding2 namedef = 
  add_in_scope namedef 

let add_binding namedef = 
  Common.profile_code "TAC.add_binding" (fun () -> add_binding2 namedef)


(*****************************************************************************)
(* Helpers, part 2 (can access semi globals) *)
(*****************************************************************************)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

let visit_toplevel elem = 

  let hooks = { Visitor_php.default_visitor with

    Visitor_php.ktop = (fun (k, vx) x -> 
      match x with
      | FuncDef def ->
          let funcalls = 
            Lib_parsing_php.get_all_funcalls_in_body (unbrace def.f_body) 
          in
          let contain_func_name_args_like = 
            no_check_when_contain +> List.exists (fun danger_func -> 
              List.mem danger_func funcalls
            )
          in
          add_binding (def.f_name, AFunc (def.f_name, 
                                         uncomma (unparen def.f_params),
                                         contain_func_name_args_like
          ));
          do_in_new_scope (fun () ->
            k x
          );
      | _ -> k x
    );

    Visitor_php.klvalue = (fun (k,vx) x ->
      match Ast_php.untype  x with
      | FunCallSimple (callname, args)  ->


          let str_name = Ast_php.name callname in

          (match lookup_env_opt str_name !_scoped_env with
          | Some (AFunc (defname, params, contain_func_num_args)) ->

              if contain_func_num_args
              then pr2 ("not checking functions containing calls to " ^
                           "func_num_args() or alike")
              else 
                check_args_vs_params 
                  (callname, unparen args) 
                  (defname, params)
          | _ -> 
              pr2 ("Didn't find function def for: " ^ str_name);
          )
      | FunCallVar _ -> pr2 "TODO: handling FuncVar";

      | _ -> k x
    );
  } in
  let visitor = Visitor_php.mk_visitor hooks in
  visitor.Visitor_php.vtop elem

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
(* catch all the decl to grow the environment *)

let check_program prog = 

  (* globals (re)initialialisation *) 
  _scoped_env := !initial_env;
  _notyped_var := (Hashtbl.create 100);

  prog +> List.iter (fun elem -> 
    visit_toplevel elem;
  )
