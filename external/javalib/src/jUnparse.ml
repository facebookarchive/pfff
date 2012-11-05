(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
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

open IO
open IO.BigEndian
open JBasics
open JBasicsLow
open JClassLow
open JParseCode
open JUnparseSignature


(* Constant pool unparsing *)
(***************************)

let unparse_constant_value ch consts = function
  | ConstString s ->
      write_ui8 ch 8;
      (write_string ch consts (jstr_raw s))
  | ConstInt i ->
      write_ui8 ch 3;
      write_real_i32 ch i
  | ConstFloat f ->
      write_ui8 ch 4;
      write_real_i32 ch (Int32.bits_of_float f)
  | ConstLong l ->
      write_ui8 ch 5;
      write_i64 ch l
  | ConstDouble d ->
      write_ui8 ch 6;
      write_double ch d
  | ConstClass c ->
      write_ui8 ch 7;
      write_string ch consts (unparse_constClass c)

let unparse_constant ch consts =
  function
    | ConstValue v -> unparse_constant_value ch consts v
    | ConstField (c, fs) ->
	write_ui8 ch 9;
	write_class ch consts c;
	write_name_and_type ch consts (fs_name fs, SValue (fs_type fs))
    | ConstMethod (c, ms) ->
	write_ui8 ch 10;
	write_object_type ch consts c;
	write_name_and_type ch consts
          (ms_name ms, SMethod (ms_args ms, ms_rtype ms))
    | ConstInterfaceMethod (c, ms) ->
	write_ui8 ch 11;
	write_class ch consts c;
	write_name_and_type ch consts
          (ms_name ms, SMethod (ms_args ms, ms_rtype ms))
    | ConstNameAndType (s, signature) ->
	write_ui8 ch 12;
	write_string ch consts s;
	write_string ch consts (unparse_descriptor signature)
    | ConstStringUTF8 s ->
	write_ui8 ch 1;
	write_string_with_length write_ui16 ch s
    | ConstUnusable -> ()

let unparse_constant_pool ch consts =
  let ch'' = output_string ()
  and i = ref 0 in
    while ! i < DynArray.length consts do
      unparse_constant ch'' consts (DynArray.unsafe_get consts ! i);
      incr i
    done;
    write_ui16 ch (DynArray.length consts);
    nwrite ch (close_out ch'')

(* Acess (and other) flags unparsing *)
(*************************************)
let class_flags =
  [`AccPublic; `AccRFU 0x2; `AccRFU 0x4; `AccRFU 0x8;
   `AccFinal; `AccSuper; `AccRFU 0x40; `AccRFU 0x80;
   `AccRFU 0x100; `AccInterface; `AccAbstract; `AccRFU 0x800;
   `AccSynthetic; `AccAnnotation; `AccEnum; `AccRFU 0x8000]
let innerclass_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccRFU 0x20; `AccRFU 0x40; `AccRFU 0x80;
   `AccRFU 0x100; `AccInterface; `AccAbstract; `AccRFU 0x800;
   `AccSynthetic; `AccAnnotation; `AccEnum; `AccRFU 0x8000]
let field_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccRFU 0x20; `AccVolatile; `AccTransient;
   `AccRFU 0x100; `AccRFU 0x200; `AccRFU 0x400; `AccRFU 0x800;
   `AccSynthetic; `AccRFU 0x2000; `AccEnum; `AccRFU 0x8000]
let method_flags =
  [`AccPublic; `AccPrivate; `AccProtected; `AccStatic;
   `AccFinal; `AccSynchronized; `AccBridge; `AccVarArgs;
   `AccNative; `AccRFU 0x200; `AccAbstract; `AccStrict;
   `AccSynthetic; `AccRFU 0x2000; `AccRFU 0x4000; `AccRFU 0x8000]

let unparse_flags all_flags flags =
  let fl = ref 0
  and fbit = ref 0 in
    List.iter
      (fun f ->
	 if List.mem f flags
	 then fl := ! fl lor (1 lsl ! fbit);
	 incr fbit)
      all_flags;
    !fl

(* Attributes unparsing *)
(************************)

let unparse_verification_type consts ch vtype =
  match vtype with
    | VTop  -> write_ui8 ch 0
    | VInteger  -> write_ui8 ch 1
    | VFloat -> write_ui8 ch 2
    | VDouble -> write_ui8 ch 3
    | VLong -> write_ui8 ch 4
    | VNull -> write_ui8 ch 5
    | VUninitializedThis -> write_ui8 ch 6
    | VObject o ->
	write_ui8 ch 7 ; write_object_type ch consts o
    | VUninitialized pc -> write_ui8 ch 8 ; write_ui16 ch pc

let unparse_verification_type_list consts ch =
  write_with_size write_ui16 ch (unparse_verification_type consts ch)

let unparse_stackmap_attribute consts stackmap =
  let ch = output_string ()
  in
    write_with_size write_ui16 ch
      (function pc, lt, st ->
	 write_ui16 ch pc;
	 unparse_verification_type_list consts ch lt;
	 unparse_verification_type_list consts ch st)
      stackmap;
    ("StackMap",close_out ch)

let unparse_stackmap_table_attribute consts stackmap_attribute =
  let ch = output_string ()
  in
    write_with_size write_ui16 ch
      (function stackmap_frame ->
	 match stackmap_frame with
	   | SameFrame k ->
	       write_ui8 ch k
	   | SameLocals (k,vtype) ->
	       write_ui8 ch k;
	       unparse_verification_type consts ch vtype
	   | SameLocalsExtended (k,offset_delta,vtype) ->
	       write_ui8 ch k;
	       write_ui16 ch offset_delta;
	       unparse_verification_type consts ch vtype
	   | ChopFrame (k,offset_delta) ->
	       write_ui8 ch k;
	       write_ui16 ch offset_delta
	   | SameFrameExtended (k,offset_delta) ->
	       write_ui8 ch k;
	       write_ui16 ch offset_delta
	   | AppendFrame (k,offset_delta,vtype_list) ->
	       (* The vtype list is not dumped with unparse_verification_type_list
		  because its length doesn't need to be dumped. Indeed it is
		  deduced from k (it is k-251). *)
	       write_ui8 ch k;
	       write_ui16 ch offset_delta;
	       List.iter (unparse_verification_type consts ch) vtype_list
	   | FullFrame (k,offset_delta,lvtypes,svtypes) ->
	       write_ui8 ch k;
	       write_ui16 ch offset_delta;
	       unparse_verification_type_list consts ch lvtypes;
	       unparse_verification_type_list consts ch svtypes
      ) stackmap_attribute;
    ("StackMapTable",close_out ch)


let rec unparse_element_value =
  let char_B = Char.code 'B'
  and char_C = Char.code 'C'
  and char_I = Char.code 'I'
  and char_S = Char.code 'S'
  and char_Z = Char.code 'Z'
  and char_D = Char.code 'D'
  and char_F = Char.code 'F'
  and char_J = Char.code 'J'
  and char_e = Char.code 'e'
  and char_s = Char.code 's'
  and char_c = Char.code 'c'
  and char_at = Char.code '@'
  and char_sqbraket = Char.code '['
  in fun consts ch -> function
    | EVCstByte cst ->
        write_ui8 ch char_B;
        write_ui16 ch (value_to_int consts (ConstInt (Int32.of_int cst)))
    | EVCstChar cst ->
        write_ui8 ch char_C;
        write_ui16 ch (value_to_int consts (ConstInt (Int32.of_int cst)))
    | EVCstInt cst ->
        write_ui8 ch char_I;
        write_ui16 ch (value_to_int consts (ConstInt cst))
    | EVCstShort cst ->
        write_ui8 ch char_S;
        write_ui16 ch (value_to_int consts (ConstInt (Int32.of_int cst)))
    | EVCstBoolean cst ->
        write_ui8 ch char_Z;
        write_ui16 ch (value_to_int consts (ConstInt (Int32.of_int cst)))
    | EVCstDouble cst ->
        write_ui8 ch char_D;
        write_ui16 ch (value_to_int consts (ConstDouble cst))
    | EVCstFloat cst ->
        write_ui8 ch char_F;
        write_ui16 ch (value_to_int consts (ConstFloat cst))
    | EVCstLong cst ->
        write_ui8 ch char_J;
        write_ui16 ch (value_to_int consts (ConstLong cst))
    | EVCstString s ->
        write_ui8 ch char_s;
        write_ui16 ch (constant_to_int consts (ConstStringUTF8 s))
    | EVEnum (cn,constructor) ->
        let type_name_index =
          constant_to_int
            consts
            (ConstStringUTF8 (unparse_object_type (TClass cn)))
        and const_name_index =
          constant_to_int consts (ConstStringUTF8 constructor)
        in
          write_ui8 ch char_e;
          write_ui16 ch type_name_index;
          write_ui16 ch const_name_index
    | EVClass value_type_option ->
        let string =
          match value_type_option with
            | None -> "V"
            | Some vt -> unparse_value_type vt
        in
          write_ui8 ch char_c;
          write_ui16 ch (constant_to_int consts (ConstStringUTF8 string))
    | EVAnnotation annot ->
        write_ui8 ch char_at;
        unparse_annotation consts ch annot
    | EVArray elements ->
        write_ui8 ch char_sqbraket;
        write_ui16 ch (List.length elements);
        List.iter (unparse_element_value consts ch) elements
and unparse_annotation consts ch annot =
  let type_index =
    let string_type = unparse_object_type (TClass annot.kind) in
      constant_to_int
        consts
        (ConstStringUTF8 string_type)
  and nb_ev_pairs = List.length annot.element_value_pairs
  in
    write_ui16 ch type_index;
    write_ui16 ch nb_ev_pairs;
    List.iter
      (fun (name,value) ->
         write_ui16 ch (constant_to_int consts (ConstStringUTF8 name));
         unparse_element_value consts ch value)
      annot.element_value_pairs

let unparse_annotations consts ch annots =
  write_ui16 ch (List.length annots);
  List.iter (unparse_annotation consts ch) annots
let unparse_parameter_annotations consts ch param_annots =
  write_ui8 ch (List.length param_annots);
  List.iter (unparse_annotations consts ch) param_annots

let rec unparse_attribute_to_strings consts =
  let ch = output_string () in
    function
      | AttributeSignature s ->
	  write_string ch consts s;
	  ("Signature",close_out ch)
      | AttributeEnclosingMethod (cn,mso) ->
	  write_class ch consts cn;
	  (match mso with
	     | Some (n,t) ->
                 write_name_and_type ch consts (n,t)
	     | None ->
		 write_ui16 ch 0);
	  ("EnclosingMethod", close_out ch)
      | AttributeSourceDebugExtension s ->
	  ("SourceDebugExtension", s)
      | AttributeSourceFile s ->
	  write_string ch consts s;
	  ("SourceFile",close_out ch)
      | AttributeConstant c ->
	  write_value ch consts c;
	  ("ConstantValue",close_out ch)
      | AttributeExceptions l ->
	  write_with_size write_ui16 ch
	    (function c -> write_class ch consts c)
	    l;
	  ("Exceptions",close_out ch)
      | AttributeInnerClasses l ->
	  write_with_size write_ui16 ch
	    (function inner, outer, inner_name, flags ->
	       (match inner with
		  | None -> write_ui16 ch 0
		  | Some inner -> write_class ch consts inner);
	       (match outer with
		  | None -> write_ui16 ch 0
		  | Some outer -> write_class ch consts outer);
	       (match inner_name with
		  | None -> write_ui16 ch 0
		  | Some inner_name ->
		      write_string ch consts inner_name);
	       write_ui16 ch (unparse_flags innerclass_flags flags))
	    l;
	  ("InnerClasses",close_out ch)
      | AttributeSynthetic ->
	  ("Synthetic",close_out ch)
      | AttributeLineNumberTable l ->
	  write_with_size write_ui16 ch
	    (function pc, line ->
	       write_ui16 ch pc;
	       write_ui16 ch line)
	    l;
	  ("LineNumberTable",close_out ch)
      | AttributeLocalVariableTable l ->
	  write_with_size write_ui16 ch
	    (function start_pc, length, name, signature, index ->
	       write_ui16 ch start_pc;
	       write_ui16 ch length;
	       write_string ch consts name;
	       write_string ch consts (unparse_value_type signature);
	       write_ui16 ch index)
	    l;
	  ("LocalVariableTable",close_out ch)
      | AttributeLocalVariableTypeTable l ->
          write_with_size write_ui16 ch
            (function start_pc, length, name, signature, index ->
               write_ui16 ch start_pc;
               write_ui16 ch length;
               write_string ch consts name;
               write_string ch consts (unparse_FieldTypeSignature signature);
               write_ui16 ch index)
            l;
          ("LocalVariableTypeTable", close_out ch)
      | AttributeDeprecated ->
	  ("Deprecated",close_out ch)
      | AttributeStackMap s ->
	  ignore (close_out ch);
	  unparse_stackmap_attribute consts s
      | AttributeStackMapTable s ->
	  ignore (close_out ch);
	  unparse_stackmap_table_attribute consts s
      | AttributeUnknown (name, contents) ->
	  (name,contents)
      | AttributeAnnotationDefault ev ->
          unparse_element_value consts ch ev;
          ("AnnotationDefault",close_out ch)
      | AttributeRuntimeVisibleAnnotations annots ->
          unparse_annotations consts ch annots;
          ("RuntimeVisibleAnnotations", close_out ch)
      | AttributeRuntimeInvisibleAnnotations annots ->
          unparse_annotations consts ch annots;
          ("RuntimeInvisibleAnnotations", close_out ch)
      | AttributeRuntimeVisibleParameterAnnotations param_annots ->
          unparse_parameter_annotations consts ch param_annots;
          ("RuntimeVisibleParameterAnnotations", close_out ch)
      | AttributeRuntimeInvisibleParameterAnnotations param_annots ->
          unparse_parameter_annotations consts ch param_annots;
          ("RuntimeInvisibleParameterAnnotations", close_out ch)
      | AttributeCode code ->
	  let code = Lazy.force code in
	    write_ui16 ch code.c_max_stack;
	    write_ui16 ch code.c_max_locals;
	    write_with_length write_i32 ch
	      (function ch ->
		 JParseCode.unparse_code ch code.c_code);
	    write_with_size write_ui16 ch
	      (function e ->
		 write_ui16 ch e.JCode.e_start;
		 write_ui16 ch e.JCode.e_end;
		 write_ui16 ch e.JCode.e_handler;
		 match e.JCode.e_catch_type with
		   | Some cl -> write_class ch consts cl
		   | None -> write_ui16 ch 0)
	      code.c_exc_tbl;
	    write_with_size write_ui16 ch
	      (unparse_attribute ch consts)
	      code.c_attributes;
	    ("Code",close_out ch)

and unparse_attribute ch consts attr =
  let (name,content) = unparse_attribute_to_strings consts attr
  in
    write_string ch consts name;
    write_string_with_length write_i32 ch content

(* Fields, methods and classes *)
(*******************************)

let unparse_field ch consts field =
  write_ui16 ch (unparse_flags field_flags field.f_flags);
  write_string ch consts field.f_name;
  write_string ch consts (unparse_value_type field.f_descriptor);
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    field.f_attributes

let unparse_method ch consts methode =
  if
    List.length
      (List.filter
	 (function AttributeCode _ -> true | _ -> false)
	 methode.m_attributes)
    > 1
  then
    raise
      (Class_structure_error "duplicate code or different versions in m_code and m_attributes");
  write_ui16 ch (unparse_flags method_flags methode.m_flags);
  write_string ch consts methode.m_name;
  write_string ch consts (unparse_method_descriptor methode.m_descriptor);
  write_with_size write_ui16 ch
    (unparse_attribute ch consts)
    methode.m_attributes

let unparse_class_low_level ch c =
  write_real_i32 ch 0xCAFEBABEl;
  write_ui16 ch c.j_version.minor;
  write_ui16 ch c.j_version.major;
  let ch' = output_string ()
  and consts = DynArray.of_array c.j_consts in
    write_ui16 ch' (unparse_flags class_flags c.j_flags);
    write_class ch' consts c.j_name;
    write_ui16 ch'
      (match c.j_super with
	 | Some super -> constant_to_int consts (ConstValue (ConstClass (TClass super)))
	 | None -> 0);
    let unparse unparse = write_with_size write_ui16 ch' unparse in
      unparse
	(function int -> write_class ch' consts int)
	c.j_interfaces;
      unparse (unparse_field ch' consts) c.j_fields;
      unparse (unparse_method ch' consts) c.j_methods;
      unparse (unparse_attribute ch' consts) c.j_attributes;
      unparse_constant_pool ch consts;
      nwrite ch (close_out ch')

let unparse_class ch c =
  unparse_class_low_level ch (JHigh2Low.high2low c)
