let vars_passed_by_ref_in_any ~in_class find_entity = 
      | FunCallSimple (name, args) ->
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
          );
          k x

      | StaticMethodCallSimple (qu, name, args) ->
          (match qu with

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
