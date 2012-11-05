(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open JBasics
open JClassLow
open JCode
open JClass

let deprecated_to_attribute = function
  | false -> []
  | true -> [AttributeDeprecated]

let synthetic_to_attribute = function
  | false -> []
  | true -> [AttributeSynthetic]

let field_generic_signature_to_attribute = function
  | None -> []
  | Some s -> [AttributeSignature (JUnparseSignature.unparse_FieldTypeSignature s)]

let method_generic_signature_to_attribute = function
  | None -> []
  | Some s -> [AttributeSignature (JUnparseSignature.unparse_MethodTypeSignature s)]

let class_generic_signature_to_attribute = function
  | None -> []
  | Some s -> [AttributeSignature (JUnparseSignature.unparse_ClassSignature s)]

let enclosingmethod_to_attribute = function
  | None -> []
  | Some (cs,mso) ->
      let meth = match mso with
	| None -> None
	| Some ms ->
	    let mname = ms_name ms in
	    let mdesc = (ms_args ms, ms_rtype ms) in
	      Some (mname, SMethod mdesc)
      in
	[AttributeEnclosingMethod (cs,meth)]

let sourcedebugextension_to_attribute = function
  | None -> []
  | Some s -> [AttributeSourceDebugExtension s]

let h2l_other_attributes l = List.map (fun (n,a) -> AttributeUnknown (n,a)) l

let h2l_attributes a =
  deprecated_to_attribute a.deprecated
  @ synthetic_to_attribute a.synthetic
  @ h2l_other_attributes a.other

let access2flags = function
  | `Default -> []
  | `Public -> [`AccPublic]
  | `Private -> [`AccPrivate]
  | `Protected -> [`AccProtected]

let h2l_inner_classes = function
  | [] -> []
  | icl ->
      let h2l_ic ic =
	let inner_class_info = ic.ic_class_name
	and outer_class_info = ic.ic_outer_class_name
	and inner_name = ic.ic_source_name
	and inner_class_access_flags =
	  (access2flags ic.ic_access)
	  @ (if ic.ic_static then [`AccStatic] else [])
	  @ (if ic.ic_final then [`AccFinal] else [])
	  @ (if ic.ic_synthetic then [`AccSynthetic] else [])
	  @ (if ic.ic_annotation then [`AccAnnotation] else [])
	  @ (if ic.ic_enum then [`AccEnum] else [])
	  @ (List.map (fun i -> `AccRFU i) ic.ic_other_flags)
	  @ (match ic.ic_type with
	       | `Interface -> [`AccAbstract;`AccInterface]
	       | `Abstract -> [`AccAbstract]
	       | `ConcreteClass -> [])
	in (inner_class_info,outer_class_info,inner_name,inner_class_access_flags)
      in
	[AttributeInnerClasses (List.rev_map h2l_ic icl)]

(* This function only build full frames. We don't try to compress the stackmap
   information like javac. *)
let h2l_stackmap_table sm : stackmap_frame list =
  let (_,table) =
    List.fold_left
      (fun (last,table) (pc,lv,sv) ->
	 if (last = 0) then
	   (pc,FullFrame(255,pc,lv,sv)::table)
	 else
	   let offset_delta = pc - last - 1 in
	     (pc,FullFrame(255,offset_delta,lv,sv)::table)
      ) (0,[]) sm in
    List.rev table

let h2l_code2attribute consts = function
  | Native -> []
  | Java code ->
      let h2l () =
	let code = Lazy.force code in
	  {JClassLow.c_max_stack = code.c_max_stack;
	   JClassLow.c_max_locals = code.c_max_locals;
	   JClassLow.c_code = JInstruction.code2opcodes consts code.c_code;
	   JClassLow.c_exc_tbl = code.c_exc_tbl;
	   JClassLow.c_attributes =
	      (match code.c_stack_map_midp with
		 | Some sm -> [AttributeStackMap sm]
		 | None -> [])
	      @ (match code.c_stack_map_java6 with
		   | Some sm -> [AttributeStackMapTable
				   (h2l_stackmap_table sm)]
		   | None -> [])
	      @ (match code.c_line_number_table with
		   | Some lnt -> [AttributeLineNumberTable lnt]
		   | None -> [])
	      @ (match code.c_local_variable_table with
		   | Some lvt -> [AttributeLocalVariableTable lvt]
		   | None -> [])
              @ (match code.c_local_variable_type_table with
                   | Some lvt -> [AttributeLocalVariableTypeTable lvt]
                   | None -> [])
	      @ h2l_other_attributes code.c_attributes;
	  }
      in
      let code = Lazy.lazy_from_fun h2l
      in
        (* bugfix : if h2l is not evaluated before the end of the main
           function (high2low), then consts is updated after c_const
           has been defined and updates of the constant pool are
           lost *)
        ignore (Lazy.force code);
        [AttributeCode (code)]

let split_vis_invis annots =
    List.fold_right
    (fun annot_vis (vis,invis) ->
       match annot_vis with
         | annot,RTVisible -> (annot::vis),invis 
         | annot,RTInvisible -> vis,(annot::invis))
    annots
    ([],[])

let h2l_annotations annots =
  let visible,invisible = split_vis_invis annots in
  let vis =
    if invisible <> []
    then [AttributeRuntimeInvisibleAnnotations invisible]
    else []
  in
    if visible <> []
    then (AttributeRuntimeVisibleAnnotations visible)::vis
    else vis

let h2l_parameter_annotation param_annots =
  let visible,invisible =
    List.fold_right
      (fun annot (vis,invis) ->
         let visible,invisible = split_vis_invis annot
         in (visible::vis),(invisible::invis))
      param_annots
      ([],[])
  in
  let vis =
    if List.exists ((<>)[]) invisible
    then [AttributeRuntimeInvisibleParameterAnnotations invisible]
    else []
  in
    if List.exists ((<>)[]) visible
    then (AttributeRuntimeVisibleParameterAnnotations visible)::vis
    else vis

let h2l_cfield _consts f =
  let fs = f.cf_signature in
  let fname = fs_name fs in
  let fdesc = fs_type fs in
    {f_name = fname;
     f_descriptor = fdesc;
     f_flags =
	(if f.cf_transient then [`AccTransient] else [])
	@ (match f.cf_kind with
	     | Final -> [`AccFinal]
	     | Volatile -> [`AccVolatile]
	     | NotFinal -> [])
	@ (if f.cf_static then [`AccStatic] else [])
	@ (if f.cf_synthetic then [`AccSynthetic] else [])
	@ (if f.cf_enum then [`AccEnum] else [])
	@ (List.map (fun i -> `AccRFU i) f.cf_other_flags)
	@ (access2flags f.cf_access);
     f_attributes =
	(match f.cf_value with 
	     Some c -> [AttributeConstant  c]
	   | None -> [] )
        @ (h2l_annotations f.cf_annotations)
	@ (field_generic_signature_to_attribute f.cf_generic_signature)
	@ (h2l_attributes f.cf_attributes);
    }

let h2l_ifield _consts f =
  let fs = f.if_signature in
  let fname = fs_name fs in
  let fdesc = fs_type fs in
    {f_name = fname;
     f_descriptor = fdesc;
     f_flags =
	(if f.if_synthetic then [`AccSynthetic] else [])
	@ (List.map (fun i -> `AccRFU i) f.if_other_flags)
	@ [`AccPublic;`AccStatic;`AccFinal];
     f_attributes =
	(match f.if_value with 
	     Some c -> [AttributeConstant c] 
	   | None -> [] )
        @ (h2l_annotations f.if_annotations)
	@ (field_generic_signature_to_attribute f.if_generic_signature)
	@ (h2l_attributes f.if_attributes);
    }

let h2l_cmethod consts m =
  let ms = m.cm_signature in
  let mname = ms_name ms in
  let mdesc = (ms_args ms, ms_rtype ms) in
  let code = h2l_code2attribute consts m.cm_implementation
  in
    {m_name = mname;
     m_descriptor = mdesc;
     m_flags =
	(if m.cm_static then [`AccStatic] else [])
	@ (if m.cm_final then [`AccFinal] else [])
	@ (if m.cm_synchronized then [`AccSynchronized] else [])
	@ (if m.cm_strict then [`AccStrict] else [])
	@ (match m.cm_implementation with Native -> [`AccNative] |_ -> [])
	@ (if m.cm_bridge then [`AccBridge] else [])
	@ (if m.cm_varargs then [`AccVarArgs] else [])
	@ (if m.cm_synthetic then [`AccSynthetic] else [])
	@ (List.map (fun i -> `AccRFU i) m.cm_other_flags)
	@ (access2flags m.cm_access);
     m_attributes =
	(match m.cm_exceptions with
	   | [] -> []
	   | l -> [AttributeExceptions l]
	)
        @ h2l_annotations m.cm_annotations.ma_global
        @ h2l_parameter_annotation m.cm_annotations.ma_parameters
	@ method_generic_signature_to_attribute m.cm_generic_signature
	@ code
	@ h2l_attributes m.cm_attributes;
    }

let h2l_amethod _consts m =
  let ms = m.am_signature in
  let mname = ms_name ms in
  let mdesc = (ms_args ms, ms_rtype ms) in
    {m_name = mname;
     m_descriptor = mdesc;
     m_flags =
	(if m.am_bridge then [`AccBridge] else [])
	@ (if m.am_varargs then [`AccVarArgs] else [])
	@ (if m.am_synthetic then [`AccSynthetic] else [])
	@ (List.map (fun i -> `AccRFU i) m.am_other_flags)
	@ (`AccAbstract::access2flags m.am_access);
     m_attributes =
	(match m.am_exceptions with
	   | [] -> []
	   | l -> [AttributeExceptions l]
	)
	@ method_generic_signature_to_attribute m.am_generic_signature
        @ h2l_annotations m.am_annotations.ma_global
        @ h2l_parameter_annotation m.am_annotations.ma_parameters
	@ h2l_attributes m.am_attributes;
    }

let h2l_acmethod consts = function
  | AbstractMethod m -> h2l_amethod consts m
  | ConcreteMethod m -> h2l_cmethod consts m

let h2l_concretemethods consts c' mm =
  {c' with
     j_methods = MethodMap.fold (fun _fs f l -> h2l_cmethod consts f::l) mm [];
  }

let h2l_methods consts c' mm =
  {c' with
     j_methods = MethodMap.fold (fun _fs f l -> h2l_acmethod consts f::l) mm [];
  }

let high2low_class c =
  let consts = DynArray.of_array c.c_consts in
  let c' =
    {j_name = c.c_name;
     j_version = c.c_version;
     j_super = c.c_super_class;
     j_interfaces = c.c_interfaces;
     j_consts = c.c_consts; (*will be updated later on*)
     j_flags =
	(if c.c_abstract then [`AccAbstract] else [])
	@ (if c.c_synthetic then [`AccSynthetic] else [])
	@ (if c.c_enum then [`AccEnum] else [])
	@ (if c.c_final then [`AccFinal] else [])
	@ (List.map (fun i -> `AccRFU i) c.c_other_flags)
	@ (`AccSuper::
	     match c.c_access with
	       | `Default -> []
	       | `Public -> [`AccPublic]);
     j_fields = FieldMap.fold (fun _fs f l -> h2l_cfield consts f::l) c.c_fields [];
     j_methods = []; (*will be set later on*)
     j_attributes =
	deprecated_to_attribute c.c_deprecated
	@ class_generic_signature_to_attribute c.c_generic_signature
	@ enclosingmethod_to_attribute c.c_enclosing_method
	@ sourcedebugextension_to_attribute c.c_source_debug_extention
        @ h2l_annotations c.c_annotations
	@ h2l_inner_classes c.c_inner_classes
	@ (match c.c_sourcefile with None -> [] | Some s -> [AttributeSourceFile s])
	@ h2l_other_attributes c.c_other_attributes;
    } in
  let c'= h2l_methods consts c' c.c_methods
  in {c' with j_consts = DynArray.to_array consts}

let high2low_interface (c:JCode.jcode jinterface) =
  let consts = DynArray.of_array c.i_consts in
  let c' =
    {j_name = c.i_name;
     j_version = c.i_version;
     j_super = Some java_lang_object;
     j_interfaces = c.i_interfaces;
     j_consts = c.i_consts; (*will be updated later on*)
     j_flags =
	(if c.i_annotation then [`AccAnnotation] else [])
	@ (List.map (fun i -> `AccRFU i) c.i_other_flags)
	@ `AccInterface::`AccAbstract::
	  (match c.i_access with
	     | `Default -> []
	     | `Public -> [`AccPublic]);
     j_fields =
	FieldMap.fold (fun _fs f l -> h2l_ifield consts f::l) c.i_fields [];
     j_methods =
	(match c.i_initializer with None -> [] | Some m -> [h2l_cmethod consts m])
	@ MethodMap.fold (fun _ms m l -> h2l_amethod consts m::l) c.i_methods [];
     j_attributes =
	deprecated_to_attribute c.i_deprecated
	@ class_generic_signature_to_attribute c.i_generic_signature
	@ sourcedebugextension_to_attribute c.i_source_debug_extention
        @ h2l_annotations c.i_annotations
	@ h2l_inner_classes c.i_inner_classes
	@ (match c.i_sourcefile with None -> [] | Some s -> [AttributeSourceFile s])
	@ h2l_other_attributes c.i_other_attributes;
    }
  in {c' with j_consts = DynArray.to_array consts}

let high2low = function
  | JInterface i -> high2low_interface i
  | JClass c -> high2low_class c
