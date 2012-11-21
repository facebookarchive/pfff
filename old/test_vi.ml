open Typedtree

type env = unit

module Ident = struct
    let t env x = 
      ()
end
module Longident = struct
    let t env x = ()
end
module Path = struct
    let t env x = ()
      
end

module Location = struct
    let t env x = ()
end

module Env = struct
    let t env x = ()
end

module Concr = struct
    let t env x = ()
end
module Meths = struct
    let t env f x = ()
end
module Primitive = struct
    let description env x = ()
end
module Types = struct
    let value_description env x = ()
    let class_declaration env x = ()
    let class_type env x = ()
    let class_signature env x = ()
    let module_type env x = ()
    let signature env x = ()
    let type_declaration env x = ()
    let exception_declaration env x = ()
    let class_type_declaration env x = ()
end

let v_list f xs = List.iter f xs
let v_option f xs = Common.do_option f xs

let v_string x = ()
let v_bool x = ()
let v_int x = ()
let v_ref f x = ()

let type_expr env x = ()
let loc f env x = ()
let constant env x = ()
let constructor_description env x = ()
let label env x = ()
let row_desc env x = ()
let label_description env x = ()
let closed_flag env x = ()
let rec_flag env x = ()
let partial env x =  ()
let optional env x = ()

let direction_flag env x = ()
let override_flag env x = ()
let mutable_flag env x = ()
let private_flag env x = ()
let virtual_flag env x = ()

let rec
  pattern env
          {
            pat_desc = v_pat_desc;
            pat_loc = v_pat_loc;
            pat_extra = v_pat_extra;
            pat_type = v_pat_type;
            pat_env = v_pat_env
          } =
  let _ = pattern_desc env v_pat_desc in
  let _ = Location.t env v_pat_loc in
  let _ =
    v_list
      (fun (v1, v2) ->
         let _ = pat_extra env v1 and _ = Location.t env v2 in ())
      v_pat_extra in
  let _ = type_expr env v_pat_type in let _ = Env.t env v_pat_env in ()
and pat_extra env =
  function
  | Tpat_constraint v1 -> let _ = core_type env v1 in ()
  | Tpat_type ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Tpat_unpack -> ()
and pattern_desc env =
  function
  | Tpat_any -> ()
  | Tpat_var ((v1, v2)) ->
      let _ = Ident.t env v1 and _ = loc env v_string v2 in ()
  | Tpat_alias ((v1, v2, v3)) ->
      let _ = pattern env v1
      and _ = Ident.t env v2
      and _ = loc env v_string v3
      in ()
  | Tpat_constant v1 -> let _ = constant env v1 in ()
  | Tpat_tuple v1 -> let _ = v_list (pattern env) v1 in ()
  | Tpat_construct ((v1, v2, v3, v4, v5)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = constructor_description env v3
      and _ = v_list (pattern env) v4
      and _ = v_bool v5
      in ()
  | Tpat_variant ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = v_option (pattern env) v2
      and _ = v_ref (row_desc env) v3
      in ()
  | Tpat_record ((v1, v2)) ->
      let _ =
        v_list
          (fun (v1, v2, v3, v4) ->
             let _ = Path.t env v1
             and _ = loc env (Longident.t env) v2
             and _ = label_description env v3
             and _ = pattern env v4
             in ())
          v1
      and _ = closed_flag env v2
      in ()
  | Tpat_array v1 -> let _ = v_list (pattern env) v1 in ()
  | Tpat_or ((v1, v2, v3)) ->
      let _ = pattern env v1
      and _ = pattern env v2
      and _ = v_option (row_desc env) v3
      in ()
  | Tpat_lazy v1 -> let _ = pattern env v1 in ()
and
  expression env
             {
               exp_desc = v_exp_desc;
               exp_loc = v_exp_loc;
               exp_extra = v_exp_extra;
               exp_type = v_exp_type;
               exp_env = v_exp_env
             } =
  let _ = expression_desc env v_exp_desc in
  let _ = Location.t env v_exp_loc in
  let _ =
    v_list
      (fun (v1, v2) ->
         let _ = exp_extra env v1 and _ = Location.t env v2 in ())
      v_exp_extra in
  let _ = type_expr env v_exp_type in let _ = Env.t env v_exp_env in ()
and exp_extra env =
  function
  | Texp_constraint ((v1, v2)) ->
      let _ = v_option (core_type env) v1
      and _ = v_option (core_type env) v2
      in ()
  | Texp_open ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = Env.t env v3
      in ()
  | Texp_poly v1 -> let _ = v_option (core_type env) v1 in ()
  | Texp_newtype v1 -> let _ = v_string v1 in ()
and expression_desc env =
  function
  | Texp_ident ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = Types.value_description env v3
      in ()
  | Texp_constant v1 -> let _ = constant env v1 in ()
  | Texp_let ((v1, v2, v3)) ->
      let _ = rec_flag env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = expression env v3
      in ()
  | Texp_function ((v1, v2, v3)) ->
      let _ = label env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = partial env v3
      in ()
  | Texp_apply ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = label env v1
             and _ = v_option (expression env) v2
             and _ = optional env v3
             in ())
          v2
      in ()
  | Texp_match ((v1, v2, v3)) ->
      let _ = expression env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = partial env v3
      in ()
  | Texp_try ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      in ()
  | Texp_tuple v1 -> let _ = v_list (expression env) v1 in ()
  | Texp_construct ((v1, v2, v3, v4, v5)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = constructor_description env v3
      and _ = v_list (expression env) v4
      and _ = v_bool v5
      in ()
  | Texp_variant ((v1, v2)) ->
      let _ = label env v1 and _ = v_option (expression env) v2 in ()
  | Texp_record ((v1, v2)) ->
      let _ =
        v_list
          (fun (v1, v2, v3, v4) ->
             let _ = Path.t env v1
             and _ = loc env (Longident.t env) v2
             and _ = label_description env v3
             and _ = expression env v4
             in ())
          v1
      and _ = v_option (expression env) v2
      in ()
  | Texp_field ((v1, v2, v3, v4)) ->
      let _ = expression env v1
      and _ = Path.t env v2
      and _ = loc env (Longident.t env) v3
      and _ = label_description env v4
      in ()
  | Texp_setfield ((v1, v2, v3, v4, v5)) ->
      let _ = expression env v1
      and _ = Path.t env v2
      and _ = loc env (Longident.t env) v3
      and _ = label_description env v4
      and _ = expression env v5
      in ()
  | Texp_array v1 -> let _ = v_list (expression env) v1 in ()
  | Texp_ifthenelse ((v1, v2, v3)) ->
      let _ = expression env v1
      and _ = expression env v2
      and _ = v_option (expression env) v3
      in ()
  | Texp_sequence ((v1, v2)) ->
      let _ = expression env v1 and _ = expression env v2 in ()
  | Texp_while ((v1, v2)) ->
      let _ = expression env v1 and _ = expression env v2 in ()
  | Texp_for ((v1, v2, v3, v4, v5, v6)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = expression env v3
      and _ = expression env v4
      and _ = direction_flag env v5
      and _ = expression env v6
      in ()
  | Texp_when ((v1, v2)) ->
      let _ = expression env v1 and _ = expression env v2 in ()
  | Texp_send ((v1, v2, v3)) ->
      let _ = expression env v1
      and _ = meth env v2
      and _ = v_option (expression env) v3
      in ()
  | Texp_new ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = Types.class_declaration env v3
      in ()
  | Texp_instvar ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = Path.t env v2
      and _ = loc env v_string v3
      in ()
  | Texp_setinstvar ((v1, v2, v3, v4)) ->
      let _ = Path.t env v1
      and _ = Path.t env v2
      and _ = loc env v_string v3
      and _ = expression env v4
      in ()
  | Texp_override ((v1, v2)) ->
      let _ = Path.t env v1
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Path.t env v1
             and _ = loc env v_string v2
             and _ = expression env v3
             in ())
          v2
      in ()
  | Texp_letmodule ((v1, v2, v3, v4)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_expr env v3
      and _ = expression env v4
      in ()
  | Texp_assert v1 -> let _ = expression env v1 in ()
  | Texp_assertfalse -> ()
  | Texp_lazy v1 -> let _ = expression env v1 in ()
  | Texp_object ((v1, v2)) ->
      let _ = class_structure env v1 and _ = v_list v_string v2 in ()
  | Texp_pack v1 -> let _ = module_expr env v1 in ()
and meth env =
  function
  | Tmeth_name v1 -> let _ = v_string v1 in ()
  | Tmeth_val v1 -> let _ = Ident.t env v1 in ()
and
  class_expr env
             {
               cl_desc = v_cl_desc;
               cl_loc = v_cl_loc;
               cl_type = v_cl_type;
               cl_env = v_cl_env
             } =
  let _ = class_expr_desc env v_cl_desc in
  let _ = Location.t env v_cl_loc in
  let _ = Types.class_type env v_cl_type in let _ = Env.t env v_cl_env in ()
and class_expr_desc env =
  function
  | Tcl_ident ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = v_list (core_type env) v3
      in ()
  | Tcl_structure v1 -> let _ = class_structure env v1 in ()
  | Tcl_fun ((v1, v2, v3, v4, v5)) ->
      let _ = label env v1
      and _ = pattern env v2
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = expression env v3
             in ())
          v3
      and _ = class_expr env v4
      and _ = partial env v5
      in ()
  | Tcl_apply ((v1, v2)) ->
      let _ = class_expr env v1
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = label env v1
             and _ = v_option (expression env) v2
             and _ = optional env v3
             in ())
          v2
      in ()
  | Tcl_let ((v1, v2, v3, v4)) ->
      let _ = rec_flag env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = expression env v3
             in ())
          v3
      and _ = class_expr env v4
      in ()
  | Tcl_constraint ((v1, v2, v3, v4, v5)) ->
      let _ = class_expr env v1
      and _ = v_option (class_type env) v2
      and _ = v_list v_string v3
      and _ = v_list v_string v4
      and _ = Concr.t env v5
      in ()
and
  class_structure env
                  {
                    cstr_pat = v_cstr_pat;
                    cstr_fields = v_cstr_fields;
                    cstr_type = v_cstr_type;
                    cstr_meths = v_cstr_meths
                  } =
  let _ = pattern env v_cstr_pat in
  let _ = v_list (class_field env) v_cstr_fields in
  let _ = Types.class_signature env v_cstr_type in
  let _ = Meths.t env (Ident.t env) v_cstr_meths in ()
and class_field env { cf_desc = v_cf_desc; cf_loc = v_cf_loc } =
  let _ = class_field_desc env v_cf_desc in
  let _ = Location.t env v_cf_loc in ()
and class_field_kind env =
  function
  | Tcfk_virtual v1 -> let _ = core_type env v1 in ()
  | Tcfk_concrete v1 -> let _ = expression env v1 in ()
and class_field_desc env =
  function
  | Tcf_inher ((v1, v2, v3, v4, v5)) ->
      let _ = override_flag env v1
      and _ = class_expr env v2
      and _ = v_option v_string v3
      and _ =
        v_list
          (fun (v1, v2) -> let _ = v_string v1 and _ = Ident.t env v2 in ())
          v4
      and _ =
        v_list
          (fun (v1, v2) -> let _ = v_string v1 and _ = Ident.t env v2 in ())
          v5
      in ()
  | Tcf_val ((v1, v2, v3, v4, v5, v6)) ->
      let _ = v_string v1
      and _ = loc env v_string v2
      and _ = mutable_flag env v3
      and _ = Ident.t env v4
      and _ = class_field_kind env v5
      and _ = v_bool v6
      in ()
  | Tcf_meth ((v1, v2, v3, v4, v5)) ->
      let _ = v_string v1
      and _ = loc env v_string v2
      and _ = private_flag env v3
      and _ = class_field_kind env v4
      and _ = v_bool v5
      in ()
  | Tcf_constr ((v1, v2)) ->
      let _ = core_type env v1 and _ = core_type env v2 in ()
  | Tcf_init v1 -> let _ = expression env v1 in ()
and
  module_expr env
              {
                mod_desc = v_mod_desc;
                mod_loc = v_mod_loc;
                mod_type = v_mod_type;
                mod_env = v_mod_env
              } =
  let _ = module_expr_desc env v_mod_desc in
  let _ = Location.t env v_mod_loc in
  let _ = Types.module_type env v_mod_type in
  let _ = Env.t env v_mod_env in ()
and module_type_constraint env =
  function
  | Tmodtype_implicit -> ()
  | Tmodtype_explicit v1 -> let _ = module_type env v1 in ()
and module_expr_desc env =
  function
  | Tmod_ident ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Tmod_structure v1 -> let _ = structure env v1 in ()
  | Tmod_functor ((v1, v2, v3, v4)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_type env v3
      and _ = module_expr env v4
      in ()
  | Tmod_apply ((v1, v2, v3)) ->
      let _ = module_expr env v1
      and _ = module_expr env v2
      and _ = module_coercion env v3
      in ()
  | Tmod_constraint ((v1, v2, v3, v4)) ->
      let _ = module_expr env v1
      and _ = Types.module_type env v2
      and _ = module_type_constraint env v3
      and _ = module_coercion env v4
      in ()
  | Tmod_unpack ((v1, v2)) ->
      let _ = expression env v1 and _ = Types.module_type env v2 in ()
and
  structure env
            {
              str_items = v_str_items;
              str_type = v_str_type;
              str_final_env = v_str_final_env
            } =
  let _ = v_list (structure_item env) v_str_items in
  let _ = Types.signature env v_str_type in
  let _ = Env.t env v_str_final_env in ()
and
  structure_item env
                 {
                   str_desc = v_str_desc;
                   str_loc = v_str_loc;
                   str_env = v_str_env
                 } =
  let _ = structure_item_desc env v_str_desc in
  let _ = Location.t env v_str_loc in let _ = Env.t env v_str_env in ()
and structure_item_desc env =
  function
  | Tstr_eval v1 -> let _ = expression env v1 in ()
  | Tstr_value ((v1, v2)) ->
      let _ = rec_flag env v1
      and _ =
        v_list
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      in ()
  | Tstr_primitive ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = value_description env v3
      in ()
  | Tstr_type v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = type_declaration env v3
             in ())
          v1
      in ()
  | Tstr_exception ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = exception_declaration env v3
      in ()
  | Tstr_exn_rebind ((v1, v2, v3, v4)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = Path.t env v3
      and _ = loc env (Longident.t env) v4
      in ()
  | Tstr_module ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_expr env v3
      in ()
  | Tstr_recmodule v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3, v4) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = module_type env v3
             and _ = module_expr env v4
             in ())
          v1
      in ()
  | Tstr_modtype ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_type env v3
      in ()
  | Tstr_open ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Tstr_class v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = class_declaration env v1
             and _ = v_list v_string v2
             and _ = virtual_flag env v3
             in ())
          v1
      in ()
  | Tstr_class_type v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = class_type_declaration env v3
             in ())
          v1
      in ()
  | Tstr_include ((v1, v2)) ->
      let _ = module_expr env v1 and _ = v_list (Ident.t env) v2 in ()
and module_coercion env =
  function
  | Tcoerce_none -> ()
  | Tcoerce_structure v1 ->
      let _ =
        v_list
          (fun (v1, v2) ->
             let _ = v_int v1 and _ = module_coercion env v2 in ())
          v1
      in ()
  | Tcoerce_functor ((v1, v2)) ->
      let _ = module_coercion env v1 and _ = module_coercion env v2 in ()
  | Tcoerce_primitive v1 -> let _ = Primitive.description env v1 in ()
and
  module_type env
              {
                mty_desc = v_mty_desc;
                mty_type = v_mty_type;
                mty_env = v_mty_env;
                mty_loc = v_mty_loc
              } =
  let _ = module_type_desc env v_mty_desc in
  let _ = Types.module_type env v_mty_type in
  let _ = Env.t env v_mty_env in let _ = Location.t env v_mty_loc in ()
and module_type_desc env =
  function
  | Tmty_ident ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Tmty_signature v1 -> let _ = signature env v1 in ()
  | Tmty_functor ((v1, v2, v3, v4)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_type env v3
      and _ = module_type env v4
      in ()
  | Tmty_with ((v1, v2)) ->
      let _ = module_type env v1
      and _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Path.t env v1
             and _ = loc env (Longident.t env) v2
             and _ = with_constraint env v3
             in ())
          v2
      in ()
  | Tmty_typeof v1 -> let _ = module_expr env v1 in ()
and
  signature env
            {
              sig_items = v_sig_items;
              sig_type = v_sig_type;
              sig_final_env = v_sig_final_env
            } =
  let _ = v_list (signature_item env) v_sig_items in
  let _ = Types.signature env v_sig_type in
  let _ = Env.t env v_sig_final_env in ()
and
  signature_item env
                 {
                   sig_desc = v_sig_desc;
                   sig_env = v_sig_env;
                   sig_loc = v_sig_loc
                 } =
  let _ = signature_item_desc env v_sig_desc in
  let _ = Env.t env v_sig_env in let _ = Location.t env v_sig_loc in ()
and signature_item_desc env =
  function
  | Tsig_value ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = value_description env v3
      in ()
  | Tsig_type v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = type_declaration env v3
             in ())
          v1
      in ()
  | Tsig_exception ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = exception_declaration env v3
      in ()
  | Tsig_module ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = module_type env v3
      in ()
  | Tsig_recmodule v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = module_type env v3
             in ())
          v1
      in ()
  | Tsig_modtype ((v1, v2, v3)) ->
      let _ = Ident.t env v1
      and _ = loc env v_string v2
      and _ = modtype_declaration env v3
      in ()
  | Tsig_open ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Tsig_include ((v1, v2)) ->
      let _ = module_type env v1 and _ = Types.signature env v2 in ()
  | Tsig_class v1 -> let _ = v_list (class_description env) v1 in ()
  | Tsig_class_type v1 ->
      let _ = v_list (class_type_declaration env) v1 in ()
and modtype_declaration env =
  function
  | Tmodtype_abstract -> ()
  | Tmodtype_manifest v1 -> let _ = module_type env v1 in ()
and with_constraint env =
  function
  | Twith_type v1 -> let _ = type_declaration env v1 in ()
  | Twith_module ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
  | Twith_typesubst v1 -> let _ = type_declaration env v1 in ()
  | Twith_modsubst ((v1, v2)) ->
      let _ = Path.t env v1 and _ = loc env (Longident.t env) v2 in ()
and
  core_type env
            {
              ctyp_desc = v_ctyp_desc;
              ctyp_type = v_ctyp_type;
              ctyp_env = v_ctyp_env;
              ctyp_loc = v_ctyp_loc
            } =
  let _ = core_type_desc env v_ctyp_desc in
  let _ = type_expr env v_ctyp_type in
  let _ = Env.t env v_ctyp_env in let _ = Location.t env v_ctyp_loc in ()
and core_type_desc env =
  function
  | Ttyp_any -> ()
  | Ttyp_var v1 -> let _ = v_string v1 in ()
  | Ttyp_arrow ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = core_type env v2
      and _ = core_type env v3
      in ()
  | Ttyp_tuple v1 -> let _ = v_list (core_type env) v1 in ()
  | Ttyp_constr ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = v_list (core_type env) v3
      in ()
  | Ttyp_object v1 -> let _ = v_list (core_field_type env) v1 in ()
  | Ttyp_class ((v1, v2, v3, v4)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = v_list (core_type env) v3
      and _ = v_list (label env) v4
      in ()
  | Ttyp_alias ((v1, v2)) ->
      let _ = core_type env v1 and _ = v_string v2 in ()
  | Ttyp_variant ((v1, v2, v3)) ->
      let _ = v_list (row_field env) v1
      and _ = v_bool v2
      and _ = v_option (v_list (label env)) v3
      in ()
  | Ttyp_poly ((v1, v2)) ->
      let _ = v_list v_string v1 and _ = core_type env v2 in ()
  | Ttyp_package v1 -> let _ = package_type env v1 in ()
and
  package_type env
               {
                 pack_name = v_pack_name;
                 pack_fields = v_pack_fields;
                 pack_type = v_pack_type;
                 pack_txt = v_pack_txt
               } =
  let _ = Path.t env v_pack_name in
  let _ =
    v_list
      (fun (v1, v2) ->
         let _ = loc env (Longident.t env) v1 and _ = core_type env v2 in ())
      v_pack_fields in
  let _ = Types.module_type env v_pack_type in
  let _ = loc env (Longident.t env) v_pack_txt in ()
and
  core_field_type env { field_desc = v_field_desc; field_loc = v_field_loc }
                  =
  let _ = core_field_desc env v_field_desc in
  let _ = Location.t env v_field_loc in ()
and core_field_desc env =
  function
  | Tcfield ((v1, v2)) -> let _ = v_string v1 and _ = core_type env v2 in ()
  | Tcfield_var -> ()
and row_field env =
  function
  | Ttag ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = v_bool v2
      and _ = v_list (core_type env) v3
      in ()
  | Tinherit v1 -> let _ = core_type env v1 in ()
and
  value_description env
                    {
                      val_desc = v_val_desc;
                      val_val = v_val_val;
                      val_prim = v_val_prim;
                      val_loc = v_val_loc
                    } =
  let _ = core_type env v_val_desc in
  let _ = Types.value_description env v_val_val in
  let _ = v_list v_string v_val_prim in
  let _ = Location.t env v_val_loc in ()
and
  type_declaration env
                   {
                     typ_params = v_typ_params;
                     typ_type = v_typ_type;
                     typ_cstrs = v_typ_cstrs;
                     typ_kind = v_typ_kind;
                     typ_private = v_typ_private;
                     typ_manifest = v_typ_manifest;
                     typ_variance = v_typ_variance;
                     typ_loc = v_typ_loc
                   } =
  let _ = v_list (v_option (loc env v_string)) v_typ_params in
  let _ = Types.type_declaration env v_typ_type in
  let _ =
    v_list
      (fun (v1, v2, v3) ->
         let _ = core_type env v1
         and _ = core_type env v2
         and _ = Location.t env v3
         in ())
      v_typ_cstrs in
  let _ = type_kind env v_typ_kind in
  let _ = private_flag env v_typ_private in
  let _ = v_option (core_type env) v_typ_manifest in
  let _ =
    v_list (fun (v1, v2) -> let _ = v_bool v1 and _ = v_bool v2 in ())
      v_typ_variance in
  let _ = Location.t env v_typ_loc in ()
and type_kind env =
  function
  | Ttype_abstract -> ()
  | Ttype_variant v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3, v4) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = v_list (core_type env) v3
             and _ = Location.t env v4
             in ())
          v1
      in ()
  | Ttype_record v1 ->
      let _ =
        v_list
          (fun (v1, v2, v3, v4, v5) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = mutable_flag env v3
             and _ = core_type env v4
             and _ = Location.t env v5
             in ())
          v1
      in ()
and
  exception_declaration env
                        {
                          exn_params = v_exn_params;
                          exn_exn = v_exn_exn;
                          exn_loc = v_exn_loc
                        } =
  let _ = v_list (core_type env) v_exn_params in
  let _ = Types.exception_declaration env v_exn_exn in
  let _ = Location.t env v_exn_loc in ()
and
  class_type env
             {
               cltyp_desc = v_cltyp_desc;
               cltyp_type = v_cltyp_type;
               cltyp_env = v_cltyp_env;
               cltyp_loc = v_cltyp_loc
             } =
  let _ = class_type_desc env v_cltyp_desc in
  let _ = Types.class_type env v_cltyp_type in
  let _ = Env.t env v_cltyp_env in let _ = Location.t env v_cltyp_loc in ()
and class_type_desc env =
  function
  | Tcty_constr ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = v_list (core_type env) v3
      in ()
  | Tcty_signature v1 -> let _ = class_signature env v1 in ()
  | Tcty_fun ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = core_type env v2
      and _ = class_type env v3
      in ()
and
  class_signature env
                  {
                    csig_self = v_csig_self;
                    csig_fields = v_csig_fields;
                    csig_type = v_csig_type;
                    csig_loc = v_csig_loc
                  } =
  let _ = core_type env v_csig_self in
  let _ = v_list (class_type_field env) v_csig_fields in
  let _ = Types.class_signature env v_csig_type in
  let _ = Location.t env v_csig_loc in ()
and class_type_field env { ctf_desc = v_ctf_desc; ctf_loc = v_ctf_loc } =
  let _ = class_type_field_desc env v_ctf_desc in
  let _ = Location.t env v_ctf_loc in ()
and class_type_field_desc env =
  function
  | Tctf_inher v1 -> let _ = class_type env v1 in ()
  | Tctf_val v1 ->
      let _ =
        (match v1 with
         | (v1, v2, v3, v4) ->
             let _ = v_string v1
             and _ = mutable_flag env v2
             and _ = virtual_flag env v3
             and _ = core_type env v4
             in ())
      in ()
  | Tctf_virt v1 ->
      let _ =
        (match v1 with
         | (v1, v2, v3) ->
             let _ = v_string v1
             and _ = private_flag env v2
             and _ = core_type env v3
             in ())
      in ()
  | Tctf_meth v1 ->
      let _ =
        (match v1 with
         | (v1, v2, v3) ->
             let _ = v_string v1
             and _ = private_flag env v2
             and _ = core_type env v3
             in ())
      in ()
  | Tctf_cstr v1 ->
      let _ =
        (match v1 with
         | (v1, v2) ->
             let _ = core_type env v1 and _ = core_type env v2 in ())
      in ()
and class_declaration env v = class_infos env (class_expr env) v
and class_description env v = class_infos env (class_type env) v
and class_type_declaration env v = class_infos env (class_type env) v
and  class_infos: 'a. env -> ('a -> unit) -> 'a class_infos -> unit = fun
 env _of_a x ->
   match x with
              {
                ci_virt = v_ci_virt;
                ci_params = v_ci_params;
                ci_id_name = v_ci_id_name;
                ci_id_class = v_ci_id_class;
                ci_id_class_type = v_ci_id_class_type;
                ci_id_object = v_ci_id_object;
                ci_id_typesharp = v_ci_id_typesharp;
                ci_expr = v_ci_expr;
                ci_decl = v_ci_decl;
                ci_type_decl = v_ci_type_decl;
                ci_variance = v_ci_variance;
                ci_loc = v_ci_loc
              } ->
  let _ = virtual_flag env v_ci_virt in
  let _ =
    match v_ci_params with
    | (v1, v2) ->
        let _ = v_list (loc env v_string) v1 and _ = Location.t env v2 in () in
  let _ = loc env v_string v_ci_id_name in
  let _ = Ident.t env v_ci_id_class in
  let _ = Ident.t env v_ci_id_class_type in
  let _ = Ident.t env v_ci_id_object in
  let _ = Ident.t env v_ci_id_typesharp in
  let _ = _of_a v_ci_expr in
  let _ = Types.class_declaration env v_ci_decl in
  let _ = Types.class_type_declaration env v_ci_type_decl in
  let _ =
    v_list (fun (v1, v2) -> let _ = v_bool v1 and _ = v_bool v2 in ())
      v_ci_variance in
  let _ = Location.t env v_ci_loc in ()

