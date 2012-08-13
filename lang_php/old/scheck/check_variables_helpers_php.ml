let vars_passed_by_ref_in_any ~in_class find_entity = 
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
