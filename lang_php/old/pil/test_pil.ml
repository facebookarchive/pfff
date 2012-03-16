(*****************************************************************************)
(* PIL *)
(*****************************************************************************)

let test_pil file =
  raise Todo
(*
  let ast = Parse_php.parse_program file in

  (* let's transform and print every expression *)
  let hooks = { V.default_visitor with
    (* old:
    V.kexpr = (fun (k, vx) e ->
      let instrs = Pil_build.linearize_expr e in
      instrs +> List.iter (fun instr ->
        pr2 (Pil.string_of_instr instr);
      );
    );
    *)
    V.kstmt = (fun (k, vx) st ->
      let stmts = Pil_build.linearize_stmt st in
      stmts +> List.iter (fun st ->
        pr2 (Meta_pil.string_of_stmt st)
      )
    );
  } in
  let v = V.mk_visitor hooks in
  v (Ast.Program ast)
*)

let test_pretty_print_pil file =
  raise Todo
(*
  let ast = Parse_php.parse_program file in
  let v = V.mk_visitor { V.default_visitor with
    V.kstmt = (fun (k, vx) st ->
      let stmts = Pil_build.linearize_stmt st in
      stmts +> List.iter (fun st ->
        pr2 (Pretty_print_pil.string_of_stmt st)
      )
    );
  } in
  v (Ast.Program ast)
*)

let test_cfg_pil file =
  raise Todo
(*
  let ast = Parse_php.parse_program file in
  ast +> List.iter (function
  | Ast_php.FuncDef def ->
      (try
         let pil = Pil_build.linearize_body (Ast.unbrace def.Ast.f_body) in
         let flow = Controlflow_build_pil.cfg_of_stmts pil in
         Controlflow_pil.display_flow flow;
      with Controlflow_build_pil.Error err ->
        Controlflow_build_pil.report_error err
      )
  | _ -> ()
  )
*)

let test_dataflow_pil file =
  raise Todo
(*
  let ast = Parse_php.parse_program file in
  ast +> List.iter (function
  | Ast_php.FuncDef def ->
      (try
         let pil = Pil_build.linearize_body (Ast.unbrace def.Ast.f_body) in
         let flow = Controlflow_build_pil.cfg_of_stmts pil in

         let reach = Dataflow_pil.reaching_fixpoint flow in
         let liveness = Dataflow_pil.liveness_fixpoint flow in
         pr "Reaching:";
         Dataflow_pil.display_reaching_dflow flow reach;
         pr "Liveness:";
         Dataflow_pil.display_liveness_dflow flow liveness

      with Controlflow_build_pil.Error err ->
        Controlflow_build_pil.report_error err
      )
  | _ -> ()
  )
*)

(* todo: adapt to PIL *)
let test_dfg_php file =
  raise Todo
(*
  let (ast2,_stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in
  ast +> List.iter (function
  | Ast_php.FuncDef def ->
      (try
        let flow = Controlflow_build_php.cfg_of_func def in
        let mapping = Dataflow_php_liveness.liveness_analysis flow in
        pr2_gen mapping
        (* Controlflow_php.display_flow flow; *)
      with Controlflow_build_php.Error err ->
        Controlflow_build_php.report_error err
      )
  | _ -> ()
  )
*)

(* collect all variables in a function using the PIL visitor *)
let test_visitor_pil file =
  raise Todo
(*
  let ast = Parse_php.parse_program file in
  ast +> List.iter (function
  | Ast_php.FuncDef def ->
      let pil = Pil_build.linearize_body (Ast.unbrace def.Ast.f_body) in
      let funcname = Ast_php.name def.Ast_php.f_name in

      let h = Hashtbl.create 101 in
      let visitor = Visitor_pil.mk_visitor { Visitor_pil.default_visitor with
        Visitor_pil.kvar = (fun (k, _) var ->
          match var with
          | Pil.Var dname ->
              let s = Ast_php.dname dname in
              Hashtbl.replace h s true
          | _ -> k var
        );
      }
      in
      visitor (Controlflow_pil.StmtList pil);
      let vars = Common.hashset_to_list h in
      pr2 (spf "vars in function %s = %s" funcname (Common.join ", " vars));
  | _ -> ()
  )
*)
