(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007,2008, 2009 Laurent Hubert (CNRS)
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
open JBasicsLow
open JClassLow
open JCode
open JClass

let debug = ref 1

(* [map2] works even if lists are not the same size by taking elements for the
   other list as default values *)
let rec map2 f l1 l2 = match l1,l2 with
  | [],[] -> []
  | [],l
  | l, [] -> l
  | e1::r1, e2::r2 -> (f e1 e2)::(map2 f r1 r2)


let rec flags2access = function
  | `AccPublic::l ->
      if List.exists (fun a -> a = `AccPrivate || a= `AccProtected) l
      then raise (Class_structure_error "Access flags Public and Private or Protected cannot be set at the same time")
      else (`Public,l)
  | `AccPrivate::l ->
      if List.exists (fun a -> a = `AccPublic || a= `AccProtected) l
      then raise (Class_structure_error "Access flags Private and Public or Protected cannot be set at the same time")
      else (`Private,l)
  | `AccProtected::l ->
      if List.exists (fun a -> a = `AccPrivate || a= `AccPublic) l
      then raise (Class_structure_error "Access flags Protected and Private or Public cannot be set at the same time")
      else (`Protected,l)
  | f::l -> let (p,fl) = flags2access l in (p,f::fl)
  | [] -> (`Default,[])

let rec get_flag flag = function
  | [] -> (false,[])
  | f::fl when f=flag -> (true,List.filter ((<>)f) fl)
  | f::fl -> let (b,fl) = get_flag flag fl in (b,f::fl)

type 's lvt = (int * int * string * 's * int) list

let combine_LocalVariableTable (lvts:'s lvt list) : 's lvt =
  let lvt = List.concat lvts in
    if not (JBasics.get_permissive ()) then
      begin
        let for_all_couple (f:'a -> 'a -> bool) (l:'a list) : bool =
	  List.for_all
	    (fun e1 -> List.for_all (f e1) l)
	    l
        and overlap (e1_start,e1_end,_,_,_) (e2_start,e2_end,_,_,_) =
	  (e2_start < e1_end && e1_start < e2_end)
        and similar (_,_,e1_name,_,e1_index) (_,_,e2_name,_,e2_index) =
	  e1_name = e2_name || e1_index = e2_index
        in
	  if not (for_all_couple (fun e1 e2 -> e1==e1 || not (overlap e1 e2 && similar e1 e2)) lvt)
	  then raise (Class_structure_error "A CodeAttribute contains more than one LocalVariableTable and they are not compatible with each other")
      end;
    lvt


(* convert a list of  attributes to a list of couple of string, as for AttributeUnknown. *)
let low2high_other_attributes consts : JClassLow.attribute list ->  (string*string) list =
  List.map
    (function
       | AttributeUnknown (name, contents) -> name, contents
       | a ->
	   let (name,contents) = JUnparse.unparse_attribute_to_strings consts a
	   in
	     if !debug >0 then prerr_endline ("Warning: unexpected attribute found: "^name);
	     name,contents)

(* convert a list of  attributes to an [attributes] structure. *)
let low2high_attributes consts (al:JClassLow.attribute list) :attributes =
  {synthetic = List.exists ((=)AttributeSynthetic) al;
   deprecated = List.exists ((=)AttributeDeprecated) al;
   other =
      low2high_other_attributes consts
	(List.filter
	   (function AttributeDeprecated | AttributeSynthetic -> false | _ -> true)
	   al);
  }

let expanse_stackmap_table stackmap_table =
  let (_,stackmap) =
    List.fold_left
      (fun ((pc,l,_),stackmap) frame ->
	 match frame with
	   | SameFrame k ->
	       let offset = pc + k + 1 in
	       let s = (offset,l,[]) in
		 (s,s::stackmap)
	   | SameLocals (k,vtype) ->
	       let offset = pc + k - 64 + 1 in
	       let s = (offset,l,[vtype]) in
		 (s,s::stackmap)
	   | SameLocalsExtended (_,offset_delta,vtype) ->
	       let offset = pc + offset_delta + 1 in
	       let s = (offset,l,[vtype]) in
		 (s,s::stackmap)
	   | ChopFrame (k,offset_delta) ->
	       let offset = pc + offset_delta + 1 in
	       let nb_chop = 251 - k in
	       let l_chop = List.rev
		 (ExtList.List.drop nb_chop (List.rev l)) in
	       let s = (offset,l_chop,[]) in
		 (s,s::stackmap)
	   | SameFrameExtended (_,offset_delta) ->
	       let offset = pc + offset_delta + 1 in
	       let s = (offset,l,[]) in
		 (s,s::stackmap)
	   | AppendFrame (_,offset_delta,vtype_list) ->
	       let offset = pc + offset_delta + 1 in
	       let s = (offset,l@vtype_list,[]) in
		 (s,s::stackmap)
	   | FullFrame (_,offset_delta,lv,sv) ->
	       let offset = pc + offset_delta + 1 in
	       let s = (offset,lv,sv) in
		 (s,s::stackmap)
      ) ((-1,[],[]),[]) stackmap_table in
    List.rev stackmap

let low2high_code consts = function c ->
  {
    c_max_stack = c.JClassLow.c_max_stack;
    c_max_locals = c.JClassLow.c_max_locals;
    c_code = JInstruction.opcodes2code (DynArray.to_array consts) c.JClassLow.c_code;
    c_exc_tbl = c.JClassLow.c_exc_tbl;
    c_line_number_table =
      begin
	let rec find_lineNumberTable = function
	  | AttributeLineNumberTable l::l' ->
	      if find_lineNumberTable l' <> None
	      then raise (Class_structure_error "Only one AttributeLineNumberTable can be attached to a method.");
	      Some l
	  | _::l -> find_lineNumberTable l
	  | [] -> None
	in find_lineNumberTable c.JClassLow.c_attributes
      end;
    c_local_variable_table =
      begin
	let lvt =
	  combine_LocalVariableTable
	    (List.fold_left
               (fun lvts ->
                  (function
                     | AttributeLocalVariableTable lvt -> lvt :: lvts
                     | _ -> lvts))
               []
               c.JClassLow.c_attributes)
	in match lvt with
	  | [] -> None
	  | _ -> Some lvt
      end;
    c_local_variable_type_table =
      begin
        let lvt = combine_LocalVariableTable
          (List.fold_left
             (fun lvts ->
                (function
                   | AttributeLocalVariableTypeTable lvt -> lvt :: lvts
                   | _ -> lvts))
             []
             c.JClassLow.c_attributes)
        in match lvt with
          | [] -> None
          | _ -> Some lvt
      end;
    c_stack_map_midp =
      begin
	let rec find_StackMap = function
	  | AttributeStackMap l :: l' ->
	      if find_StackMap l' <> None
	      then raise (Class_structure_error "Only one StackMap attribute can be attached to a method.");
	      Some l
	  | _::l -> find_StackMap l
	  | [] -> None
	in find_StackMap c.JClassLow.c_attributes
      end;
    c_stack_map_java6 =
      begin
	let rec find_StackMapTable = function
	  | AttributeStackMapTable l :: l' ->
	      if find_StackMapTable l' <> None
	      then raise (Class_structure_error "Only one StackMapTable attribute can be attached to a method.");
	      Some (expanse_stackmap_table l)
	  | _::l -> find_StackMapTable l
	  | [] -> None
	in find_StackMapTable c.JClassLow.c_attributes
      end;
    c_attributes = low2high_other_attributes consts
      (List.filter
	 (function
	    | AttributeStackMap _
	    | AttributeStackMapTable _ | AttributeLocalVariableTable _
	    | AttributeLineNumberTable _ -> false
	    | _ -> true)
	 c.JClassLow.c_attributes);
  }

let low2high_cfield cn consts fs = function f ->
  let flags = f.f_flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (access,flags) = flags2access flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_volatile,flags) = get_flag `AccVolatile flags in
  let (is_transient,flags) = get_flag `AccTransient flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_enum,flags) = get_flag `AccEnum flags in
  let flags =
    List.map (function
		| `AccRFU i -> i
		| _ ->
		    prerr_endline "unexcepted flag in JLow2High.low2high_cfield: bug in JavaLib";
		    assert false)
      flags
  in
  let kind =
    if is_final
    then
      if not (JBasics.get_permissive ()) && is_volatile
      then raise (Class_structure_error "A field cannot be final and volatile.")
      else Final
    else
      if is_volatile then Volatile else NotFinal
  in
  let (cst,other_att) =
    List.partition (function AttributeConstant _ -> true | _ -> false) f.f_attributes in
  let (cst,other_att) =
    match cst with
      | [] -> None,other_att
      | AttributeConstant c::oc when not is_static ->  (* it seems quite common *)
	  if !debug > 1
          then
            prerr_endline
              ("Warning: Non-static field "^JDumpBasics.class_name cn ^ "."
               ^ fs_name fs ^ " has been found with a constant value associated.");
	  None, (AttributeConstant c::(oc@other_att))
      | AttributeConstant c::[] ->
	  Some c, other_att
      | AttributeConstant c::oc ->
	  if !debug > 0
          then
            prerr_endline
              ("Warning: Field " ^ JDumpBasics.class_name cn ^ "."
               ^ fs_name fs ^ " contains more than one constant value associated.");
	  Some c, (oc@other_att)
      | _ -> assert false
  in
  let (generic_signature,other_att) =
    List.partition (function AttributeSignature _ -> true | _ -> false) other_att in
  let generic_signature =
    match generic_signature with
      | [] -> None
      | (AttributeSignature s)::rest  ->
          if rest = [] || JBasics.get_permissive ()
          then
	    try
	      Some (JParseSignature.parse_FieldTypeSignature s)
	    with Class_structure_error _ as e ->
	      if JBasics.get_permissive ()
	      then None
	      else raise e
	  else
            raise (Class_structure_error "A field contains more than one Signature attribute asscociated with it.")
      | _ -> assert false
  in
  let (annotations,other_att) =
    List.partition
      (function
         | AttributeRuntimeVisibleAnnotations _
         | AttributeRuntimeInvisibleAnnotations _
           -> true
         | _ -> false)
      other_att
  in let annotations =
      List.fold_right
        (fun annot annots ->
           match annot with
             | AttributeRuntimeVisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTVisible)::annots)
                   al
                   annots
             | AttributeRuntimeInvisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTInvisible)::annots)
                   al
                   annots
             | _ -> assert false)
        annotations
        []
  in
    {
      cf_signature = fs;
      cf_class_signature = make_cfs cn fs;
      cf_generic_signature = generic_signature;
      cf_access = access;
      cf_static = is_static;
      cf_kind = kind;
      cf_value = cst;
      cf_transient = is_transient;
      cf_synthetic = is_synthetic;
      cf_enum = is_enum;
      cf_other_flags = flags;
      cf_annotations = annotations;
      cf_attributes =
	low2high_attributes consts other_att;
    }

let low2high_ifield cn consts fs = function f ->
  let flags = f.f_flags in
  let (is_public,flags) = get_flag `AccPublic flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
    if not(is_public && is_static && is_final)
    then raise (Class_structure_error "A field of an interface must be : Public, Static and Final.");
    let flags = List.map
      (function
	 | `AccRFU i -> i
	 | _ -> raise (Class_structure_error "A field of an interface may only have it `AccSynthetic flag set in addition of `AccPublic, `AccStatic and `AccFinal."))
      flags
    in
    let (generic_signature,other_att) =
      List.partition (function AttributeSignature _ -> true | _ -> false) f.f_attributes in
    let generic_signature =
      match generic_signature with
	| [] -> None
	| (AttributeSignature s)::rest ->
            if rest = [] || JBasics.get_permissive ()
	    then
	      try
		Some (JParseSignature.parse_FieldTypeSignature s)
	      with Class_structure_error _ as e ->
		if JBasics.get_permissive ()
		then None
		else raise e
	    else
            raise (Class_structure_error "A field contains more than one Signature attribute asscociated with it.")
        | _ -> assert false
    in
    let (csts,other_att) =
      List.partition (function AttributeConstant _ -> true | _ -> false) other_att in
    let cst = match csts with
      | [] -> None
      | [AttributeConstant c] -> Some c
      | _ -> raise (Class_structure_error "An interface field contains more than one Constant Attribute.")
    in
    let (annotations,other_att) =
      List.partition
        (function
           | AttributeRuntimeVisibleAnnotations _
           | AttributeRuntimeInvisibleAnnotations _
             -> true
           | _ -> false)
        other_att
    in let annotations =
        List.fold_right
          (fun annot annots ->
             match annot with
               | AttributeRuntimeVisibleAnnotations al ->
                   List.fold_right
                     (fun a annots -> (a,RTVisible)::annots)
                     al
                     annots
               | AttributeRuntimeInvisibleAnnotations al ->
                   List.fold_right
                     (fun a annots -> (a,RTInvisible)::annots)
                     al
                     annots
               | _ -> assert false)
          annotations
          []
    in
      {
	if_signature = fs;
        if_class_signature = make_cfs cn fs;
	if_generic_signature = generic_signature;
	if_value = cst;
	if_synthetic = is_synthetic;
	if_other_flags = flags;
        if_annotations = annotations;
	if_attributes = low2high_attributes consts other_att;
      }

let low2high_amethod consts cs ms = function m ->
  let flags = m.m_flags in
  let (access,flags) = flags2access flags in
  let (_is_abstract,flags) = get_flag `AccAbstract flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_bridge,flags) = get_flag `AccBridge flags in
  let (is_varargs,flags) = get_flag `AccVarArgs flags in
  let access =
    match access with
      | `Private -> raise (Class_structure_error "Abstract method cannot be private")
      | `Default -> `Default
      | `Protected -> `Protected
      | `Public -> `Public
  in
  let flags =
    List.map
      (function
	 | `AccRFU i -> i
	 | _ -> raise (Class_structure_error (
			 "If a method has its ACC_ABSTRACT flag set it may not have any"
			 ^ "of its ACC_FINAL, ACC_NATIVE, ACC_PRIVATE, ACC_STATIC, "
			 ^ "ACC_STRICT, or ACC_SYNCHRONIZED flags set.")))
      flags
  in
  let (generic_signature, other_att) =
    List.partition (function AttributeSignature _ -> true | _ -> false) m.m_attributes in
  let generic_signature = match generic_signature with
    | [] -> None
    | (AttributeSignature s)::rest ->
        if rest = [] || JBasics.get_permissive ()
	then
	  try
	    Some (JParseSignature.parse_MethodTypeSignature s)
	  with Class_structure_error _ as e ->
	    if JBasics.get_permissive ()
	    then None
	    else raise e
	else
          raise (Class_structure_error "An abstract method cannot have several Signature attributes.")
    | _ -> assert false
  in
  let (exn,other_att) =
    List.partition (function AttributeExceptions _-> true | _ -> false) other_att in
  let exn = match exn with
    | [] -> []
    | [AttributeExceptions cl] -> cl
    | _ -> raise (Class_structure_error "Only one Exception attribute is allowed in a method.")
  in
  let (default_annotation,other_att) =
    List.partition
      (function | AttributeAnnotationDefault _ -> true | _ -> false)
      other_att
  in
  let default_annotation =
    match default_annotation with
      | [] -> None
      | [AttributeAnnotationDefault ad] -> Some ad
      | _::_::_ ->
          raise (Class_structure_error
                   "A method should not have more than one AnnotationDefault attribute")
      | [_] -> assert false
  in
  let (annotations,other_att) =
    List.partition
      (function
         | AttributeRuntimeVisibleAnnotations _
         | AttributeRuntimeInvisibleAnnotations _
           -> true
         | _ -> false)
      other_att
  in let annotations =
      List.fold_right
        (fun annot annots ->
           match annot with
             | AttributeRuntimeVisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTVisible)::annots)
                   al
                   annots
             | AttributeRuntimeInvisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTInvisible)::annots)
                   al
                   annots
             | _ -> assert false)
        annotations
        []
  in
  let (parameter_annotations,other_att) =
    List.partition
      (function
         | AttributeRuntimeVisibleParameterAnnotations _
         | AttributeRuntimeInvisibleParameterAnnotations _
           -> true
         | _ -> false)
      other_att
  in
  let parameter_annotations =
    if parameter_annotations = []
    then []
    else
      let res =
        List.fold_left
          (fun (res:'a list) -> function
             | AttributeRuntimeVisibleParameterAnnotations pa ->
                 let pa = List.map (List.map (fun a -> (a,RTVisible))) pa
                 in map2 (@) pa res
             | AttributeRuntimeInvisibleParameterAnnotations pa ->
                 let pa = List.map (List.map (fun a -> (a,RTInvisible))) pa
                 in map2 (@) pa res
             | _ -> assert false)
          []
          parameter_annotations
      in
        if List.length res <=  List.length (JBasics.ms_args ms)
        then res
        else raise
          (Class_structure_error
             "The length of an Runtime(In)VisibleParameterAnnotations \
              is longer than the number of arguments of the same method")
  in
    {
      am_signature = ms;
      am_class_method_signature = make_cms cs ms;
      am_access = access;
      am_generic_signature = generic_signature;
      am_synthetic = is_synthetic;
      am_bridge = is_bridge;
      am_varargs = is_varargs;
      am_other_flags = flags;
      am_exceptions = exn;
      am_attributes = low2high_attributes consts other_att;
      am_annotations =
        {ma_global = annotations;
         ma_parameters = parameter_annotations;};
      am_annotation_default = default_annotation;
    }

let low2high_cmethod consts cs ms = function m ->
  if m.m_name = "<init>" &&
    List.exists (fun a -> a=`AccStatic || a=`AccFinal || a=`AccSynchronized || a=`AccNative || a=`AccAbstract)
    m.m_flags
  then raise (Class_structure_error ("A specific instance initialization method may have at most "
				     ^ "one of its ACC_PRIVATE, ACC_PROTECTED, and ACC_PUBLIC flags set "
				     ^ "and may also have its ACC_STRICT flag set."));
  let flags = m.m_flags in
  let (access,flags) = flags2access flags in
  let (is_static,flags) = get_flag `AccStatic flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_synchronized,flags) = get_flag `AccSynchronized flags in
  let (is_strict,flags) = get_flag `AccStrict flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_bridge,flags) = get_flag `AccBridge flags in
  let (is_varargs,flags) = get_flag `AccVarArgs flags in
  let (is_native,flags) = get_flag `AccNative flags in
  let flags = List.map
    (function
       | `AccRFU i -> i
       | `AccAbstract -> raise (Class_structure_error "Non abstract class cannot have abstract methods.")
       | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_cmethod : unexpected flag found."))
    flags
  in
  let (generic_signature,other_att) =
    List.partition (function AttributeSignature _ -> true | _ -> false) m.m_attributes in
  let generic_signature = match generic_signature with
    | [] -> None
    | (AttributeSignature s)::rest ->
        if rest = [] || JBasics.get_permissive ()
	then
	  try
	    Some (JParseSignature.parse_MethodTypeSignature s)
	  with Class_structure_error _ as e ->
	    if JBasics.get_permissive ()
	    then None
	    else raise e
        else
          raise (Class_structure_error "A method cannot have several Signature attributes.")
    | _ -> assert false
  and (exn,other_att) =
    List.partition (function AttributeExceptions _ -> true | _ -> false) other_att in
  let exn = match exn with
    | [] -> []
    | [AttributeExceptions cl] -> cl
    | _ -> raise (Class_structure_error "Only one Exception attribute is allowed in a method.")
  and (code,other_att) =
    List.partition (function AttributeCode _ -> true | _ -> false) other_att in
  let code = match code with
    | [AttributeCode c] when not is_native ->
	Java (lazy (low2high_code consts (Lazy.force c)))
    | [] when is_native -> Native
    | [] ->
        raise
          (Class_structure_error
             "A method not declared as Native, nor Abstract has been found without code.")
    | [_] ->
        raise
          (Class_structure_error
             "A method declared as Native has been found with a code attribute.")
    | _::_::_ ->
        raise
          (Class_structure_error
             "Only one Code attribute is allowed in a method.")
  in
  let (annotations,other_att) =
    List.partition
      (function
         | AttributeRuntimeVisibleAnnotations _
         | AttributeRuntimeInvisibleAnnotations _
           -> true
         | _ -> false)
      other_att
  in let annotations =
      List.fold_right
        (fun annot annots ->
           match annot with
             | AttributeRuntimeVisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTVisible)::annots)
                   al
                   annots
             | AttributeRuntimeInvisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTInvisible)::annots)
                   al
                   annots
             | _ -> assert false)
        annotations
        []
  in
  let (parameter_annotations,other_att) =
    List.partition
      (function
         | AttributeRuntimeVisibleParameterAnnotations _
         | AttributeRuntimeInvisibleParameterAnnotations _
           -> true
         | _ -> false)
      other_att
  in
  let parameter_annotations =
    if parameter_annotations = []
    then []
    else
      let res =
        List.fold_left
          (fun (res:'a list) -> function
             | AttributeRuntimeVisibleParameterAnnotations pa ->
                 let pa = List.map (List.map (fun a -> (a,RTVisible))) pa
                 in map2 (@) pa res
             | AttributeRuntimeInvisibleParameterAnnotations pa ->
                 let pa = List.map (List.map (fun a -> (a,RTInvisible))) pa
                 in map2 (@) pa res
             | _ -> assert false)
          []
          parameter_annotations
      in
        if List.length res <=  List.length (JBasics.ms_args ms)
        then res
        else raise
          (Class_structure_error
             "The length of an Runtime(In)VisibleParameterAnnotations \
              is longer than the number of arguments of the same method")
  in
    {
      cm_signature = ms;
      cm_class_method_signature = make_cms cs ms;
      cm_static = is_static;
      cm_final = is_final;
      cm_synchronized = is_synchronized;
      cm_strict = is_strict;
      cm_access = access;
      cm_generic_signature = generic_signature;
      cm_bridge = is_bridge;
      cm_varargs = is_varargs;
      cm_synthetic = is_synthetic;
      cm_other_flags = flags;
      cm_exceptions = exn;
      cm_attributes = low2high_attributes consts other_att;
      cm_annotations =
        {ma_global = annotations;
         ma_parameters = parameter_annotations;};
      cm_implementation = code;
    }

let low2high_acmethod consts cs ms = function m ->
  if List.exists ((=)`AccAbstract) m.m_flags
  then AbstractMethod (low2high_amethod consts cs ms m)
  else ConcreteMethod (low2high_cmethod consts cs ms m)

let low2high_methods cn consts = function ac ->
  let cs = ac.j_name in
    List.fold_left
      (fun map meth ->
	 let ms = make_ms meth.m_name (fst meth.m_descriptor) (snd meth.m_descriptor) in
	   if !debug > 0 && MethodMap.mem ms map
	   then
	     prerr_endline
	       ("Warning: in " ^ JDumpBasics.class_name cn
                ^ " 2 methods have been found with the same signature (" ^ meth.m_name
		^"("^ String.concat ", " (List.map (JDumpBasics.value_signature) (fst meth.m_descriptor)) ^"))");
	   MethodMap.add
	     ms
	     (try low2high_acmethod consts cs ms meth
	      with Class_structure_error msg ->
		raise (Class_structure_error
			 ("in method " ^JDumpBasics.signature meth.m_name (SMethod meth.m_descriptor)^": "^msg)))
	     map
      ) MethodMap.empty ac.j_methods

let low2high_innerclass = function
    (inner_class_info,outer_class_info,inner_name,flags) ->
      let (access,flags) = flags2access flags in
      let (is_final,flags) = get_flag `AccFinal flags in
      let (is_static,flags) = get_flag `AccStatic flags in
      let (is_interface,flags) = get_flag `AccInterface flags in
      let (is_abstract,flags) = get_flag `AccAbstract flags in
      let (is_synthetic,flags) = get_flag `AccSynthetic flags in
      let (is_annotation,flags) = get_flag `AccAnnotation flags in
      let (is_enum,flags) = get_flag `AccEnum flags in
      let flags = List.map
	(function
	  | `AccRFU i -> i
	  | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_cmethod : unexpected flag found."))
	flags
      in
	{
	  ic_class_name = inner_class_info;
	  ic_outer_class_name = outer_class_info;
	  ic_source_name = inner_name;
	  ic_access = access;
	  ic_static = is_static;
	  ic_final = is_final;
	  ic_synthetic = is_synthetic;
	  ic_annotation = is_annotation;
	  ic_enum = is_enum;
	  ic_other_flags = flags;
	  ic_type =
	    if is_interface
	    then `Interface
	    else
	      if is_abstract
	      then `Abstract
	    else `ConcreteClass
	}

let low2high_class cl =
  if cl.j_super = None && cl.j_name <> JBasics.java_lang_object
  then raise (Class_structure_error "Only java.lang.Object is allowed not to have a super-class.");
  let cs = cl.j_name in
  let flags = cl.j_flags in
  let (access,flags) = flags2access (flags :> access_flag list) in
  let (accsuper,flags) = get_flag `AccSuper flags in
  let (is_final,flags) = get_flag `AccFinal flags in
  let (is_interface,flags) = get_flag `AccInterface flags in
  let (is_abstract,flags) = get_flag `AccAbstract flags in
  let (is_synthetic,flags) = get_flag `AccSynthetic flags in
  let (is_annotation,flags) = get_flag `AccAnnotation flags in
  let (is_enum,flags) = get_flag `AccEnum flags in
  let flags =
    List.map
      (function
	 | `AccRFU i -> i
	 | _ -> raise (Failure "Bug in JavaLib in JLow2High.low2high_class : unexpected flag found."))
      flags
  in
    if not (JBasics.get_permissive ())
      && not (accsuper || is_interface)
      && not (accsuper && is_interface)
    then raise (Class_structure_error "ACC_SUPER must be set for all classes (that are not interfaces)");
    if not (JBasics.get_permissive ()) && (is_final && is_abstract)
    then raise (Class_structure_error "An abstract class cannot be final.");
    let consts = DynArray.of_array cl.j_consts in
    let my_name = cl.j_name in
    let my_version = cl.j_version in
    let my_access =
      match access with
	| `Public -> `Public
	| `Default -> `Default
	| _ -> raise (Class_structure_error "Invalid visibility for a class.")
    and my_interfaces = cl.j_interfaces
    and my_sourcefile =
      let rec find_SourceFile = function
	| AttributeSourceFile s::_ -> Some s
	| _::l -> find_SourceFile l
	| [] -> None
      in find_SourceFile cl.j_attributes
    and my_deprecated = List.exists ((=)AttributeDeprecated) cl.j_attributes
    and my_generic_signature =
      match List.find_all (function AttributeSignature _ -> true| _ -> false) cl.j_attributes with
	| [] -> None
	| (AttributeSignature s)::rest  ->
            if rest = [] || JBasics.get_permissive ()
            then
	      try
		Some (JParseSignature.parse_ClassSignature s)
	      with Class_structure_error _ as e ->
		if JBasics.get_permissive ()
		then None
		else raise e
            else
              raise (Class_structure_error "A class or interface cannot have several Signature attributes.")
        | _ -> assert false
    and my_source_debug_extention =
      let sde_attributes =
	List.find_all
	  (function AttributeSourceDebugExtension _ -> true | _ -> false)
	  cl.j_attributes
      in
	match sde_attributes with
	  | [] -> None
	  | (AttributeSourceDebugExtension s)::rest
            ->
              if rest = [] || JBasics.get_permissive ()
              then Some s
              else
                raise
		  (Class_structure_error
		     "A class cannot contain several SourceDebugExtension attribute.")
          | _ -> assert false
    and my_inner_classes =
      let rec find_InnerClasses = function
	| AttributeInnerClasses icl::_ -> List.rev_map low2high_innerclass icl
	| _::l -> find_InnerClasses l
	| [] -> []
      in find_InnerClasses cl.j_attributes
    and my_annotations =
      List.fold_right
        (fun annot annots ->
           match annot with
             | AttributeRuntimeVisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTVisible)::annots)
                   al
                   annots
             | AttributeRuntimeInvisibleAnnotations al ->
                 List.fold_right
                   (fun a annots -> (a,RTInvisible)::annots)
                   al
                   annots
             | _ -> annots)
        cl.j_attributes
        []
    and my_other_attributes =
      low2high_other_attributes consts
	(List.filter
	   (function
	      | AttributeSignature _ | AttributeSourceFile _
              | AttributeSourceDebugExtension _
              | AttributeRuntimeVisibleAnnotations _
              | AttributeRuntimeInvisibleAnnotations _
	      | AttributeDeprecated | AttributeInnerClasses _ -> false
	      | AttributeEnclosingMethod _ -> is_interface
	      | _ -> true)
	   cl.j_attributes);
    in
      if is_interface
      then
	begin
          if not (JBasics.get_permissive ())
          then
            begin
	      if not is_abstract
	      then raise (Class_structure_error
                            "A class file with its `AccInterface flag set must \
                             also have its `AccAbstract flag set.");
	      if not (cl.j_super = Some JBasics.java_lang_object)
	      then raise (Class_structure_error
                            "The super-class of interfaces must be java.lang.Object.");
	      if is_enum
	      then raise (Class_structure_error
                            "A class file with its `AccInterface flag set must \
                             not have  its their `AccEnum flag set.")
            end;
	  let (init,methods) =
	    match
	      List.partition
		(fun m ->
		   let clinit_name = ms_name clinit_signature in
		   let clinit_desc = (ms_args clinit_signature, ms_rtype clinit_signature) in
		     m.m_name = clinit_name
		       && fst m.m_descriptor = fst clinit_desc)
		cl.j_methods
	    with
	      | [m],others -> Some (low2high_cmethod consts cs clinit_signature m),others
	      | [],others -> None, others
	      | m::_::_,others ->
		  if not (JBasics.get_permissive ())
		  then raise (Class_structure_error
                                "has more than one class initializer <clinit>")
		  else Some (low2high_cmethod consts cs clinit_signature m),others
	  in
	    JInterface {
	      i_name = my_name;
	      i_version = my_version;
	      i_access = my_access;
	      i_generic_signature = my_generic_signature;
	      i_interfaces = my_interfaces;
	      i_consts = DynArray.to_array consts;
	      i_sourcefile = my_sourcefile;
	      i_deprecated = my_deprecated;
	      i_source_debug_extention = my_source_debug_extention;
	      i_inner_classes = my_inner_classes;
	      i_other_attributes = my_other_attributes;
	      i_initializer = init;
	      i_annotation = is_annotation;
              i_annotations = my_annotations;
	      i_other_flags = flags;
	      i_fields = List.fold_left
		(fun m f ->
		   let fs = make_fs f.f_name f.f_descriptor in
		     if !debug > 0 && FieldMap.mem fs m
		     then
		       prerr_endline
			 ("Warning: in " ^ JDumpBasics.class_name my_name
                          ^ " 2 fields have been found with the same signature ("
			  ^ JDumpBasics.value_signature f.f_descriptor
                          ^ " " ^ f.f_name ^ ")");
		     FieldMap.add
		       fs
		       (try low2high_ifield my_name consts fs f
			with Class_structure_error msg ->
			  raise (Class_structure_error ("field "
                                                        ^ (JDumpBasics.signature
                                                             f.f_name
                                                             (SValue f.f_descriptor))
                                                        ^ ": " ^ msg)))
		       m)
		FieldMap.empty
		cl.j_fields;
	      i_methods = List.fold_left
		(fun map meth ->
		   let ms =
                     make_ms
                       meth.m_name
                       (fst meth.m_descriptor)
                       (snd meth.m_descriptor)
                   in
		     if !debug > 0 && MethodMap.mem ms map
		     then
		       prerr_endline
			 ("Warning: in " ^ JDumpBasics.class_name my_name
                          ^ " 2 methods have been found with the same signature ("
                          ^ meth.m_name ^"("
                          ^ String.concat ", " (List.map (JDumpBasics.value_signature)
						  (fst meth.m_descriptor))
                          ^ "))");
		     MethodMap.add
		       ms
		       (try low2high_amethod consts cs ms meth
			with Class_structure_error msg ->
			  let sign =
                            JDumpBasics.signature
                              meth.m_name
                              (SMethod meth.m_descriptor)
			  in raise (Class_structure_error
                                      ("in class " ^ JDumpBasics.class_name my_name
                                       ^ ": method " ^ sign ^ ": " ^ msg)))
		       map)
		MethodMap.empty
		methods;
	    }
	end
      else
	begin
	  if is_annotation
	  then
            raise
              (Class_structure_error
                 "Class file with their `AccAnnotation flag set must also have their `AccInterface flag set.");
	  let my_enclosing_method =
            let enclosing_method_atts =
              List.find_all
                (function AttributeEnclosingMethod _ -> true | _ -> false)
                cl.j_attributes
            in
	      match enclosing_method_atts  with
	        | [] -> None
	        | [AttributeEnclosingMethod (cs,mso)] ->
		    let ms =
		      match mso with
		        | None -> None
		        | Some (mn,SMethod mdesc) ->
			    Some (make_ms mn (fst mdesc) (snd mdesc))
		        | Some (_,SValue _) ->
			    raise
			      (Class_structure_error
			         "A EnclosingMethod attribute cannot specify a field as enclosing method.")
		    in Some (cs, ms)
	        | _ ->
		    raise
		      (Class_structure_error
		         "A EnclosingMethod attribute can only be specified at most once per class.")
	  and my_methods =
	    try low2high_methods my_name consts cl
	    with
	      | Class_structure_error msg ->
		  raise (Class_structure_error
			   ("in class "^JDumpBasics.class_name my_name^": "^msg))
	  and my_fields =
	    List.fold_left
	      (fun m f ->
		 let fs = make_fs f.f_name f.f_descriptor in
		   if !debug > 0 && FieldMap.mem fs m
		   then
		     prerr_endline
		       ("Warning: in " ^ JDumpBasics.class_name my_name
                        ^ " 2 fields have been found with the same signature ("
			^JDumpBasics.value_signature f.f_descriptor^" "^f.f_name ^")");
		   FieldMap.add fs
		     (try low2high_cfield my_name consts fs f
		      with Class_structure_error msg ->
			raise (Class_structure_error
				 ("in class "^JDumpBasics.class_name my_name
				  ^": in field " ^
				  JDumpBasics.signature f.f_name
				  (SValue f.f_descriptor)^": "^msg))
		     ) m)
	      FieldMap.empty
	      cl.j_fields
	  in
	    JClass {
	      c_name = my_name;
	      c_version = my_version;
	      c_super_class = cl.j_super;
	      c_generic_signature = my_generic_signature;
	      c_final = is_final;
	      c_abstract = is_abstract;
	      c_access = my_access;
	      c_synthetic = is_synthetic;
	      c_enum = is_enum;
	      c_other_flags = flags;
	      c_interfaces = my_interfaces;
	      c_consts = DynArray.to_array consts;
	      c_sourcefile = my_sourcefile;
	      c_deprecated = my_deprecated;
	      c_source_debug_extention = my_source_debug_extention;
	      c_enclosing_method = my_enclosing_method;
              c_annotations = my_annotations;
	      c_inner_classes = my_inner_classes;
	      c_other_attributes = my_other_attributes;
	      c_fields = my_fields;
	      c_methods = my_methods;
	    }
	end

let low2high_class cl =
  try low2high_class cl
  with Class_structure_error msg ->
    raise (Class_structure_error ("In " ^  JDumpBasics.class_name cl.j_name ^ ": " ^ msg))
