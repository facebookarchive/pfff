(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module E = Database_code
module G = Graph_code

open Cmt_format
open Typedtree

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for ML compiled objects. See graph_code.ml
 * and main_codegraph.ml for more information.
 * 
 * As opposed to lang_ml/analyze/graph_code_ml.ml, no need for:
 *  - module lookup (all names are resolved)
 *  - multiple parameters, everything is curried (fun x y --> fun x -> fun y)
 * 
 * schema:
 *  Root -> Dir -> Module -> ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  current: Graph_code.node;
  phase: phase;

  current_qualifier: string;
}
 and phase = Defs | Uses

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse file =
  Common.memoized _hmemo file (fun () ->
    Cmt_format.read_cmt file
  )

let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | File_type.Obj "cmt" -> true
    | _ -> false
  ) +> Common.sort

let add_use_edge env dst =
  let src = env.current in
  match () with
  (* maybe nested function, in which case we dont have the def *)
  | _ when not (G.has_node src env.g) ->
    pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist (nested func?)"
           (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst env.g -> 
    G.add_edge (src, dst) G.Use env.g
  | _ -> 
    let (str, kind) = dst in
    (match kind with
    | _ ->
      let kind_original = kind in
      let dst = (str, kind_original) in
      
      G.add_node dst env.g;
      let parent_target = G.not_found in
      pr2 (spf "PB: lookup fail on %s (in %s)" 
             (G.string_of_node dst) (G.string_of_node src));
          
      env.g +> G.add_edge (parent_target, dst) G.Has;
      env.g +> G.add_edge (src, dst) G.Use;
    )

let todo () = pr2_once "TODO"

let rec kind_of_core_type x =
  match x.ctyp_desc with
  | Ttyp_any  | Ttyp_var _
      -> raise Todo
  | Ttyp_arrow _ -> E.Function
  | _ -> raise Todo

let kind_of_value_descr vd =
  kind_of_core_type vd.val_desc




module Ident = struct
    let t env x = 
      ()
    let name = Ident.name
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

let v_option f xs = Common.do_option f xs

let v_string x = ()
let v_bool x = ()
let v_int x = ()
let v_ref f x = ()

let meth env x = ()
let class_structure env x = ()

let module_type env x = ()
let module_coercion env x = ()
let module_type_constraint env x = ()

let type_expr env x = ()
let loc f env x = ()
let constant env x = ()
let constructor_description env x = ()
let label env x = ()
let row_desc env x = ()
let label_description env x = ()
let partial env x =  ()
let optional env x = ()

let closed_flag env x = ()
let rec_flag env x = ()
let direction_flag env x = ()
let private_flag env x = ()
let mutable_flag env x = ()
  
(*****************************************************************************)
(* Defs/Uses *)
(*****************************************************************************)
let rec extract_defs_uses ~phase ~g ~ast ~readable =
  let env = {
    g; phase;
    current = (ast.cmt_modname, E.Module);
    current_qualifier = ast.cmt_modname;
  }
  in
  if phase = Defs then begin
    let dir = Common.dirname readable in
    G.create_intermediate_directories_if_not_present g dir;
    g +> G.add_node env.current;
    g +> G.add_edge ((dir, E.Dir), env.current) G.Has;
  end;
  if phase = Uses then begin
    ast.cmt_imports +> List.iter (fun (s, digest) ->
      let node = (s, E.Module) in
      add_use_edge env node
    );
  end;
  binary_annots env ast.cmt_annots

and binary_annots env = function
  | Implementation s -> structure env s
  | Interface _
  | Packed _ 
  | Partial_implementation _ | Partial_interface _ ->
    pr2_gen env.current;
    raise Todo

(* ---------------------------------------------------------------------- *)
(* Structure *)
(* ---------------------------------------------------------------------- *)
and structure env 
 { str_items = v_str_items;  str_type = v_str_type; str_final_env = v_str_final_env } =
  let _ = List.iter (structure_item env) v_str_items in
  let _ = Types.signature env v_str_type in
  let _ = Env.t env v_str_final_env in ()
and structure_item env
                 {
                   str_desc = v_str_desc;
                   str_loc = v_str_loc;
                   str_env = v_str_env
                 } =
  let _ = structure_item_desc env v_str_desc in
  let _ = Location.t env v_str_loc in let _ = Env.t env v_str_env in ()
and structure_item_desc env =
  function
  | (Tstr_class _|Tstr_class_type _) -> todo()
  | Tstr_eval v1 -> let _ = expression env v1 in ()
  | Tstr_value ((v1, v2)) ->
      let _ = rec_flag env v1
      and _ =
        List.iter
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      in ()
  | Tstr_primitive ((id, _loc, vd)) ->
    let full_ident = env.current_qualifier ^ "." ^ Ident.name id in
    let node = (full_ident, kind_of_value_descr vd) in
    if env.phase = Defs then begin
      env.g +> G.add_node node;
      env.g +> G.add_edge (env.current, node) G.Has;
    end;
    let env = { env with  current = node; current_qualifier = full_ident; } in
    value_description env vd

  | Tstr_type v1 ->
      List.iter (fun (id, _loc, v3) ->
       let full_ident = env.current_qualifier ^ "." ^ Ident.name id in
       let node = (full_ident, E.Type) in
       if env.phase = Defs then begin
         env.g +> G.add_node node;
         env.g +> G.add_edge (env.current, node) G.Has;
       end;
       let env = { env with  current = node; current_qualifier = full_ident; }in
       type_declaration env v3
      ) v1

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
        List.iter
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
  | Tstr_include ((v1, v2)) ->
      let _ = module_expr env v1 and _ = List.iter (Ident.t env) v2 in ()

(* ---------------------------------------------------------------------- *)
(* Pattern *)
(* ---------------------------------------------------------------------- *)
and  pattern env
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
    List.iter
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
  | Tpat_tuple v1 -> let _ = List.iter (pattern env) v1 in ()
  | Tpat_construct ((v1, v2, v3, v4, v5)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = constructor_description env v3
      and _ = List.iter (pattern env) v4
      and _ = v_bool v5
      in ()
  | Tpat_variant ((v1, v2, v3)) ->
      let _ = label env v1
      and _ = v_option (pattern env) v2
      and _ = v_ref (row_desc env) v3
      in ()
  | Tpat_record ((v1, v2)) ->
      let _ =
        List.iter
          (fun (v1, v2, v3, v4) ->
             let _ = Path.t env v1
             and _ = loc env (Longident.t env) v2
             and _ = label_description env v3
             and _ = pattern env v4
             in ())
          v1
      and _ = closed_flag env v2
      in ()
  | Tpat_array v1 -> let _ = List.iter (pattern env) v1 in ()
  | Tpat_or ((v1, v2, v3)) ->
      let _ = pattern env v1
      and _ = pattern env v2
      and _ = v_option (row_desc env) v3
      in ()
  | Tpat_lazy v1 -> let _ = pattern env v1 in ()
(* ---------------------------------------------------------------------- *)
(* Expression *)
(* ---------------------------------------------------------------------- *)
and expression env
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
    List.iter
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
        List.iter
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = expression env v3
      in ()
  | Texp_function ((v1, v2, v3)) ->
      let _ = label env v1
      and _ =
        List.iter
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = partial env v3
      in ()
  | Texp_apply ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        List.iter
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
        List.iter
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      and _ = partial env v3
      in ()
  | Texp_try ((v1, v2)) ->
      let _ = expression env v1
      and _ =
        List.iter
          (fun (v1, v2) ->
             let _ = pattern env v1 and _ = expression env v2 in ())
          v2
      in ()
  | Texp_tuple v1 -> let _ = List.iter (expression env) v1 in ()
  | Texp_construct ((v1, v2, v3, v4, v5)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = constructor_description env v3
      and _ = List.iter (expression env) v4
      and _ = v_bool v5
      in ()
  | Texp_variant ((v1, v2)) ->
      let _ = label env v1 and _ = v_option (expression env) v2 in ()
  | Texp_record ((v1, v2)) ->
      let _ =
        List.iter
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
  | Texp_array v1 -> let _ = List.iter (expression env) v1 in ()
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
        List.iter
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
      let _ = class_structure env v1 and _ = List.iter v_string v2 in ()
  | Texp_pack v1 -> let _ = module_expr env v1 in ()
(* ---------------------------------------------------------------------- *)
(* Module *)
(* ---------------------------------------------------------------------- *)
and module_expr env
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
(* ---------------------------------------------------------------------- *)
(* Type *)
(* ---------------------------------------------------------------------- *)
and core_type env
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
  | Ttyp_tuple v1 -> let _ = List.iter (core_type env) v1 in ()
  | Ttyp_constr ((v1, v2, v3)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = List.iter (core_type env) v3
      in ()
  | Ttyp_object v1 -> let _ = List.iter (core_field_type env) v1 in ()
  | Ttyp_class ((v1, v2, v3, v4)) ->
      let _ = Path.t env v1
      and _ = loc env (Longident.t env) v2
      and _ = List.iter (core_type env) v3
      and _ = List.iter (label env) v4
      in ()
  | Ttyp_alias ((v1, v2)) ->
      let _ = core_type env v1 and _ = v_string v2 in ()
  | Ttyp_variant ((v1, v2, v3)) ->
      let _ = List.iter (row_field env) v1
      and _ = v_bool v2
      and _ = v_option (List.iter (label env)) v3
      in ()
  | Ttyp_poly ((v1, v2)) ->
      let _ = List.iter v_string v1 and _ = core_type env v2 in ()
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
    List.iter
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
      and _ = List.iter (core_type env) v3
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
  let _ = List.iter v_string v_val_prim in
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
  let _ = List.iter (v_option (loc env v_string)) v_typ_params in
  let _ = Types.type_declaration env v_typ_type in
  let _ =
    List.iter
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
    List.iter (fun (v1, v2) -> let _ = v_bool v1 and _ = v_bool v2 in ())
      v_typ_variance in
  let _ = Location.t env v_typ_loc in ()
and type_kind env =
  function
  | Ttype_abstract -> ()
  | Ttype_variant v1 ->
      let _ =
        List.iter
          (fun (v1, v2, v3, v4) ->
             let _ = Ident.t env v1
             and _ = loc env v_string v2
             and _ = List.iter (core_type env) v3
             and _ = Location.t env v4
             in ())
          v1
      in ()
  | Ttype_record v1 ->
      let _ =
        List.iter
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
  let _ = List.iter (core_type env) v_exn_params in
  let _ = Types.exception_declaration env v_exn_exn in
  let _ = Location.t env v_exn_loc in ()

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir_or_file skip_list =
  let root = Common.realpath dir_or_file in
  let all_files = 
    find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let readable = Common.filename_without_leading_path root file in
      extract_defs_uses ~g ~ast ~phase:Defs ~readable;
      ()
    ));

  (* step2: creating the 'Use' edges *)
  if verbose then pr2 "\nstep2: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse file in
      let readable = Common.filename_without_leading_path root file in
      if readable =~ "^external" || readable =~ "^EXTERNAL"
      then ()
      else extract_defs_uses ~g ~ast ~phase:Uses ~readable;
      ()
    ));

  g
