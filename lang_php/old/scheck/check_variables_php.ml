let check_undefined_variable ~in_lambda var env = 
        (if in_lambda 
        then (E.UseOfUndefinedVariableInLambda s)
        else 
            let allvars = Env.collect_all_vars env +> List.map Ast.dname in
            let suggest = Suggest_fix_php.suggest s allvars in
            (E.UseOfUndefinedVariable (s, suggest))

let do_in_new_scope_and_check_unused_if_strict f =
  if !E.strict 
  then do_in_new_scope_and_check_unused f
  (* otherwise use same scope *)
  else f ()
 
(*****************************************************************************)
(* Scoped visitor *)
(*****************************************************************************)

let visit_prog find_entity prog = 

  let in_lambda = ref false in

  let visitor = Visitor_php.mk_visitor { Visitor_php.default_visitor with

    (* -------------------------------------------------------------------- *)
    (* scoping management *)
    (* -------------------------------------------------------------------- *)

    (* function scope checking *)
    V.kfunc_def = (fun (k, _) x ->
        match x.f_type with
        | MethodAbstract _ -> 
            (* we don't want parameters in method interface to be counted
             * as unused Parameter *)
            ()

        | MethodRegular ->
              (* we put 1 as 'use_count' below because we are not interested
               * in error message related to $this.
               * It's legitimate to not use $this in a method.
               *)
              let dname = Ast.DName ("this", Ast.fakeInfo "this") in
              add_binding dname (S.Class, ref 1);
            end;
            do_in_new_scope_and_check_unused (fun () -> k x);
      | FunctionRegular | FunctionLambda -> 
          do_in_new_scope_and_check_unused (fun () -> k x);
      ))
    );

    (* 'if', 'while', and other blocks should introduce a new scope.
     * The default function-only scope of PHP is a bad idea. 
     * Jslint thinks the same. Even if PHP has no good scoping rules, 
     * I want to force programmers like in Javascript to write code
     * that assumes good scoping rules.
     * 
     * Note that this will just create a scope and check for the
     * blocks in a a function. You also need a do_in_new_scope_and_check()
     * for the function itself which can have parameters that we
     * want to add in the environment and check for unused.
     *)
    V.kstmt_and_def_list_scope = (fun (k, _) x ->
      do_in_new_scope_and_check_unused_if_strict (fun () -> k x)
    );

    (* 
     * Introduce a new scope for StmtList ? This would forbid user to 
     * have some statements, a func, and then more statements
     * that share the same variable. Toplevel statements
     * should not be mixed with function definitions (excepts
     * for the include/require at the top, which anyway are
     * not really, and should not be statements)
     *)

    (* -------------------------------------------------------------------- *)
    (* adding defs of dname in environment *)
    (* -------------------------------------------------------------------- *)

    V.kstmt = (fun (k, vx) x ->
      match x with
      | Globals (_, vars_list, _) ->
          vars_list +> Ast.uncomma +> List.iter (fun var ->
            match var with
            | GlobalVar dname -> 
                add_binding dname (S.Global, ref 0)
            | GlobalDollar (tok, _)  | GlobalDollarExpr (tok, _) ->
                E.warning tok E.UglyGlobalDynamic
          )

      | StaticVars (_, vars_list, _) ->
          vars_list +> Ast.uncomma +> List.iter (fun (varname, affect_opt) ->
            add_binding varname (S.Static, ref 0);
            (* TODO recurse on the affect ? *)
          )

    (* -------------------------------------------------------------------- *)
    (* checking uses *)
    (* -------------------------------------------------------------------- *)

    V.kexpr = (fun (k, vx) x ->
      match x with

      (* todo? if the ConsList is not at the toplevel, then 
       * the code below will be executed first, which means
       * vars_used_in_expr will wrongly think vars in list() expr
       * are unused var. But this should be rare because list()
       * should be used at a statement level!
       *)
      | AssignList (_, xs, _, e) ->

          let assigned = xs +> Ast.unparen +> Ast.uncomma in
          (* Use the same trick than for LocalIterator *)
          let shared_ref = ref 0 in

          assigned +> List.iter (fun list_assign ->
            let vars = vars_used_in_any (ListAssign list_assign) in
            vars +> List.iter (fun v ->
              (* if the variable was already existing, then 
               * better not to add a new binding cos this will mask
               * a previous one which will never get its ref 
               * incremented.
               *)
              let s = Ast.dname v in
              match lookup_env_opt s !_scoped_env with
              | None ->
                  add_binding v (S.ListBinded, shared_ref)
              | Some _ ->
                  ()
            );
          );
          vx (Expr e)

      | Eval _ ->
          Common.save_excursion bailout true (fun () ->
            k x
          )
      | Lambda (l_use, def) ->
          (* reset completely the environment *)
          Common.save_excursion _scoped_env !initial_env (fun () ->
          Common.save_excursion in_lambda true (fun () ->
          Common.save_excursion is_top_expr true (fun () ->
            do_in_new_scope_and_check_unused (fun () ->
              ...
            )
          
      | _ ->

       (* do the checking and environment update only for the top expressions *)
       if !is_top_expr
       then begin
        is_top_expr := false;
      
        let used = 
          vars_used_in_any (Expr x) in
        let assigned = 
          vars_assigned_in_any (Expr x) in

        (* todo: enough? if do $x = $x + 1 then have problems? *)
        let used' = 
          used +> Common.exclude (fun v -> 
            List.mem v assigned || List.mem v passed_by_refs 
             (* actually passing a var by def can also mean it's used
              * as the variable can be a 'inout', but this special
              * case is handled specifically for passed_by_refs
              *)
          )
        in

        used' +> List.iter (fun v -> 
          check_undefined_variable ~in_lambda:!in_lambda ~bailout:!bailout
            v !_scoped_env
        );

        k x;
        is_top_expr := true;
      end
       else
         (* still recurse when not in top expr *)
         k x
    );

    V.klvalue = (fun (k,vx) x ->
      match x with

      (* the checking is done upward, in kexpr, and only for topexpr *)
      | Var (dname, scope_ref) ->

          (* assert scope_ref = S.Unknown ? *)
          let s = Ast.dname dname in
    
          (match lookup_env_opt s !_scoped_env with
          | None -> scope_ref := S.Local;
          | Some (scope, _) -> scope_ref := scope;
          )
