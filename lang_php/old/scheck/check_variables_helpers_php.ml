(* update: does consider also function calls to function taking parameters via
 * reference. Use global info.
 *)
let vars_assigned_in_any any =
  any +> V.do_visit_with_ref (fun aref -> { V.default_visitor with
    V.kexpr = (fun (k,vx) x ->
      match x with
      | Assign (lval, _, _) 
      | AssignOp (lval, _, _) 
      | AssignRef (lval, _, _, _) 
      | AssignNew (lval, _, _, _, _,  _) ->
          let vopt = get_assigned_var_lval_opt lval in
          vopt |> Common.do_option (fun v -> Common.push2 v aref);
          (* recurse, can have $x = $y = 1 *)
          k x
      | _ -> 
          k x
    );
    })

(* 
 * less: maybe could be merged with vars_assigned_in but maybe we want
 * the caller to differentiate between regular assignements
 * and possibly assigned by being passed by ref
 * 
 * todo: factorize code, at least for the methods and new calls.
 *)
let vars_passed_by_ref_in_any ~in_class find_entity = 
  V.do_visit_with_ref (fun aref -> 
    let params_vs_args params args = 

      let params = params +> Ast.unparen +> Ast.uncomma_dots in
      let args = 
        match args with
        | None -> []
        | Some args -> args +> Ast.unparen +> Ast.uncomma 
      in

      (* maybe the #args does not match #params, but this is not our
       * business here; this will be detected anyway by check_functions or
       * check_class
       *)
      Common.zip_safe params args +> List.iter (fun (param, arg) ->
        match arg with
        | Arg (Lv((Var(dname, _scope)))) ->
            if param.p_ref <> None
            then Common.push2 dname aref
        | _ -> ()
      );
    in
    
  { V.default_visitor with
    V.klvalue = (fun (k, vx) x ->
      match x with
      (* quite similar to code in check_functions_php.ml *)

      | FunCallSimple (name, args) ->
          let s = Ast.name name in
          (match s with
          (* special case, ugly but hard to do otherwise *)
          | "sscanf" -> 
              (match args +> Ast.unparen +> Ast.uncomma with
              | x::y::vars ->
                  vars +> List.iter (fun arg ->
                    match arg with
                    | Arg (Lv((Var(dname, _scope)))) ->
                        Common.push2 dname aref
                    (* todo? wrong, it should be a variable *)
                    | _ -> ()
                  )
              (* wrong number of arguments, not our business, it will
               * be detected by another checker anyway
               *)
              | _ -> ()
              )
          | s -> 
           E.find_entity_and_warn find_entity (Ent.Function, name) 
             (function Ast_php.FunctionE def ->
                params_vs_args def.f_params (Some args)
             | _ -> raise Impossible
           )
          );
          k x
      | StaticMethodCallSimple (qu, name, args) ->
          (match qu with
          | ClassName (classname), _ ->
              let aclass = Ast.name classname in
              let amethod = Ast.name name in
              (try 
                let def = Class_php.lookup_method ~case_insensitive:true
                  (aclass, amethod) find_entity 
                in
                params_vs_args def.f_params (Some args)
             (* could not find the method, this is bad, but
              * it's not our business here; this error will
              * be reported anyway in check_functions_php.ml anyway
              *)
              with 
              | Not_found | Multi_found 
              | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ -> ()
              )
          | (Self _ | Parent _), _ ->
              (* The code of traits can contain reference to Parent:: that
               * we cannot unsugar.
               * todo: check that in_trait here? or avoid calling
               * this function when inside traits?
               *)
              (* failwith "check_var_help: call unsugar_self_parent()"*)
              ()
          | LateStatic _, _ ->
              (* not much we can do :( *)
              ()
          );
          k x

      | MethodCallSimple (lval, _tok, name, args) ->
          (match lval with
          (* if this-> then can use lookup_method.
           * Being complete and handling any method calls like $o->foo()
           * requires to know what is the type of $o which is quite
           * complicated ... so let's skip that for now.
           * 
           * todo: special case also id(new ...)-> ?
           *)
          | This _ ->
              (match in_class with
              | Some aclass ->
                let amethod = Ast.name name in
                (try 
                  let def = Class_php.lookup_method ~case_insensitive:true
                    (aclass, amethod) find_entity 
                  in
                  params_vs_args def.f_params (Some args)
                with 
                | Not_found | Multi_found
                | Class_php.Use__Call|Class_php.UndefinedClassWhileLookup _ ->()
                )
              | None ->
                  (* wtf? use of $this outside a class? *)
                  ()
              )
          | _ -> ()
          );
          k x 

      | FunCallVar _ -> 
          (* can't do much *)
          k x
      | _ -> 
          k x
    );
    V.kexpr = (fun (k, vx) x ->
      (match x with
      | New (tok, class_name_ref, args) ->
          (match class_name_ref with
          | ClassNameRefStatic (ClassName name) ->
              E.find_entity_and_warn find_entity (Ent.Class Ent.RegularClass, 
                                                 name)
              (function Ast_php.ClassE def ->
                (try 
                    (* TODO: use lookup_method there too ! *)
                    let ctor_def = Class_php.get_constructor def in
                    params_vs_args ctor_def.f_params args
                  (* not our business *)
                  with Not_found -> ()
                );
              | _ -> raise Impossible
              )
          | ClassNameRefStatic (Self _ | Parent _) ->
              (* failwith "check_var_help: call unsugar_self_parent()" *)
              ()
          (* can't do much *)
          | ClassNameRefDynamic _  | ClassNameRefStatic (LateStatic _) -> ()
          )
      | _ -> ()
      );
      k x
    );
    }
  )
