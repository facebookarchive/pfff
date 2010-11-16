
    let visitor =
      V.mk_visitor { V.default_visitor with
        V.klvalue = (fun (k, _) x ->
          match Ast.untype x with
          | FunCallSimple (funcname, args) ->
              (let ids = Database_php.function_ids__of_string
                       (Ast.name funcname) db in
               match ids with
              | [] ->
                  report_warning (UndefinedFunction funcname)
              | _ :: _ :: _ ->
                  (* a function with the same name is defined at different places *)
                  (* TODO: deal with functions defined several times *)
                  report_warning (UnableToDetermineDef funcname)
              | [id] ->
                  let def = match Database_php.ast_of_id id db with
                    | Ast_entity_php.Function(def) -> def
                    | _ -> raise Impossible
                  in
                  let rec aux params args =
                    match (params, args) with
                    | [], [] -> ()
                    | [], y::ys ->
                        report_warning (TooManyArguments(funcname, def));
                        aux [] ys
                    | x::xs, [] ->
                        (match x.p_default with
                        | None ->
                            report_warning (TooFewArguments(funcname, def));
                        | Some _ -> ()
                        );
                        aux xs []
                    | x::xs, y::ys ->
                        (match y with
                        | Arg(Assign((Var(dn, _), _),_ , expr), _) ->
                            if not (Ast.dname dn =$= Ast.dname x.p_name)
                            then
                              report_warning
                                (WrongKeywordArgument(dn, expr, funcname,
                                                      x, def))
                        | _ -> ()
                        );
                        aux xs ys
                  in

                  let params = def.f_params in
                  aux (Ast.uncomma (Ast.unparen params))
                      (Ast.uncomma (Ast.unparen args))
              );
              k x
          | _ -> k x
        );
      }
    in
    visitor (Program ast)
