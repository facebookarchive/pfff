open Ast_fuzzy_opa

let vof_token t =
  Ocaml.VString (Token_helpers_opa.str_of_tok t)

let vof_wrap f v =
  match v with
  (x, _tok) -> f x

let vof_name =
  function
  | Name v1 ->
      let v1 = vof_wrap Ocaml.vof_string v1 in Ocaml.VSum (("Name", [ v1 ]))

let rec vof_long_name (v1, v2) =
  let v1 = vof_qualifier v1 and v2 = vof_name v2 in Ocaml.VTuple [ v1; v2 ]
and vof_qualifier v = Ocaml.vof_list vof_name v

let rec vof_tree =
  function
  | Function v1 ->
      let v1 = vof_func_def v1 in Ocaml.VSum (("Function", [ v1 ]))
  | TypeDef ((v1, v2)) ->
      let v1 = vof_name v1
      and v2 = vof_type_def v2
      in Ocaml.VSum (("TypeDef", [ v1; v2 ]))
  | Module ((v1, v2)) ->
      let v1 = vof_name v1
      and v2 = Ocaml.vof_list vof_tree v2
      in Ocaml.VSum (("Module", [ v1; v2 ]))
  | VarDef ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_type_ v1
      and v2 = vof_name v2
      in Ocaml.VSum (("VarDef", [ v1; v2 ]))

 | TreeTodo -> Ocaml.VSum (("TreeTodo", []))

  | T v1 -> let v1 = vof_token v1 in Ocaml.VSum (("T", [ v1 ]))
  | Paren v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Paren", [ v1 ]))
  | Brace v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Brace", [ v1 ]))
  | Bracket v1 ->
      let v1 = Ocaml.vof_list (Ocaml.vof_list vof_tree) v1
      in Ocaml.VSum (("Bracket", [ v1 ]))
  | Xml ((v1, v2)) ->
      let v1 = Ocaml.vof_list vof_tree v1
      and v2 = Ocaml.vof_list vof_tree v2
      in Ocaml.VSum (("Xml", [ v1; v2 ]))

and vof_type_def =
  function
  | TyRecord v1 ->
      let v1 = Ocaml.vof_list vof_field_decl v1
      in Ocaml.VSum (("TyRecord", [ v1 ]))
  | TypeDefOther v1 ->
      let v1 = Ocaml.vof_list vof_tree v1
      in Ocaml.VSum (("TypeDefOther", [ v1 ]))
and vof_field_decl =
  function
  | Field ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_type_ v1
      and v2 = vof_name v2
      in Ocaml.VSum (("Field", [ v1; v2 ]))
  | FieldOther v1 ->
      let v1 = Ocaml.vof_list vof_tree v1
      in Ocaml.VSum (("FieldOther", [ v1 ]))

and vof_func_def {
                 f_name = v_f_name;
                 f_ret_type = v_f_ret_type;
                 f_params = v_f_params;
                 f_body = v_f_body
               } =
  let bnds = [] in
  let arg = vof_body v_f_body in
  let bnd = ("f_body", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_list vof_parameter v_f_params in
  let bnd = ("f_params", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option vof_type_ v_f_ret_type in
  let bnd = ("f_ret_type", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_option vof_name v_f_name in
  let bnd = ("f_name", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
and vof_parameter =
  function
  | Param ((v1, v2)) ->
      let v1 = Ocaml.vof_option vof_type_ v1
      and v2 = vof_name v2
      in Ocaml.VSum (("Param", [ v1; v2 ]))
  | ParamOther v1 ->
      let v1 = Ocaml.vof_list vof_tree v1
      in Ocaml.VSum (("ParamOther", [ v1 ]))
and vof_body v = Ocaml.vof_list vof_tree v
and vof_type_ =
  function
  | TyName v1 -> let v1 = vof_long_name v1 in Ocaml.VSum (("TyName", [ v1 ]))
  | TyVar v1 -> let v1 = vof_name v1 in Ocaml.VSum (("TyVar", [ v1 ]))
  | TyApp ((v1, v2)) ->
      let v1 = vof_long_name v1
      and v2 = Ocaml.vof_list vof_type_ v2
      in Ocaml.VSum (("TyApp", [ v1; v2 ]))
  | TyOther v1 ->
      let v1 = Ocaml.vof_list vof_tree v1 in Ocaml.VSum (("TyOther", [ v1 ]))

  
let vof_tree_list xs = Ocaml.vof_list vof_tree xs
