(*
 * This file is part of Javalib
 * Copyright (c)2009, Frederic Dabrowski (INRIA)
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

exception JDumpJasminException of string

let string_of_version version  =
  (string_of_int version.major)^(".")^(string_of_int version.minor)

let string_of_classname cn =  JDumpBasics.class_name ~jvm:true cn

let string_of_classname_type cn = JDumpBasics.object_value_signature ~jvm:true (TClass cn)

let string_of_class_flags flags =
  let to_string flag = match flag with
    | `AccPublic -> "public"
    | `AccSynthetic -> "synthetic"
    | `AccFinal  -> "final"
    | `AccAbstract -> "abstract"
    | `AccAnnotation -> "annotation"
    | `AccEnum -> "enum"
    | `AccInterface  -> "interface"
    | `AccSuper -> ""
    | _ -> ""
  in String.concat " " (List.map to_string flags)

let string_of_field_flags flags =
  let to_string flag = match flag with
    | `AccPublic -> "public"
    | `AccPrivate -> "private"
    | `AccProtected -> "protected"
    | `AccStatic -> "static"
    | `AccFinal -> "final"
    | `AccVolatile -> "volatile"
    | `AccTransient -> "transient"
    | `AccSynthetic -> "synthetic"
    | `AccEnum -> "enum"
    | _ -> ""
  in String.concat " " (List.map to_string flags)

let string_of_method_flags flags =
  let to_string flag = match flag with
    | `AccPublic -> "public"
    | `AccPrivate -> "private"
    | `AccProtected -> "protected"
    | `AccStatic -> "static"
    | `AccFinal -> "final"
    | `AccSynchronized -> "synchronized"
    | `AccBridge -> "bridge"
    | `AccVarArgs -> "varargs"
    | `AccNative -> "native"
    | `AccAbstract -> "abstract"
    | `AccStrict -> ""
    | _ -> ""
  in String.concat " " (List.map to_string flags)

let string_of_value_type = JDumpBasics.value_signature ~jvm:true

let string_of_object_type = JDumpBasics.object_value_signature ~jvm:true

let string_objtype_2_classname = 
  function 
      TClass cn -> string_of_classname cn
    | TArray _ as ot -> string_of_object_type ot

let string_of_method_descriptor (l,r) =
  let params = String.concat "" (List.map string_of_value_type l)
  and return = JDumpBasics.rettype2shortstring ~jvm:true r
  in "("^params^")"^return

let string_of_constant_value v = match v with
  | ConstInt i -> Int32.to_string i
  | ConstLong i -> Int64.to_string i
  | ConstFloat f | ConstDouble f -> string_of_float f
  | ConstString str -> "\""^(jstr_pp str)^"\""
  | ConstClass (TClass cn) -> string_of_classname cn
  | ConstClass (TArray _ as ot) -> string_of_object_type ot

let string_of_constant c = match c with
  | ConstValue(c) -> string_of_constant_value c
  | ConstField(classname, fs) ->
      let classname' = string_of_classname classname
      and fielddesc' = string_of_value_type (fs_type fs)
      in classname'^"/"^fs_name fs^" "^fielddesc'
  | ConstMethod(objecttype,ms) ->
      let objecttype' = string_objtype_2_classname objecttype
      and methoddesc' = string_of_method_descriptor (ms_args ms, ms_rtype ms)
      in objecttype'^"/"^(ms_name ms)^methoddesc'
  | ConstInterfaceMethod(classname, ms) ->
      let classname' = string_of_classname classname
      and methoddesc' = string_of_method_descriptor (ms_args ms, ms_rtype ms)
      in classname'^"/"^(ms_name ms)^methoddesc'
  | ConstNameAndType(str,desc) ->
      let desc' =
	match desc with
	  | SValue d -> string_of_value_type d
	  | SMethod d -> string_of_method_descriptor d
      in str^" "^desc'
  | ConstStringUTF8(str) -> str
  | ConstUnusable -> "##### Unusable Constant #####"

let string_of_opcode opcode constants pos =
    match opcode with
      | OpNop -> "nop"
      | OpAConstNull -> "aconst_null"
      | OpIConst(i) ->
	  if (Int32.to_int i >= 0) then "iconst_"^(Int32.to_string i)
	  else "iconst_m1"
      | OpLConst(i) -> "lconst_"^(Int64.to_string i)
      | OpFConst(f) -> "fconst_"^(string_of_int (int_of_float f))
      | OpDConst(d) -> "dconst_"^(string_of_int (int_of_float d))
      | OpBIPush(i) -> "bipush "^(string_of_int i)
      | OpSIPush(i) -> "sipush "^(string_of_int i)
      | OpLdc1(i) -> "ldc "^(string_of_constant constants.(i))
      | OpLdc1w(i) -> "ldc_w "^(string_of_constant constants.(i))
      | OpLdc2w(i) -> "ldc2_w "^(string_of_constant constants.(i))

      | OpLoad(jbt,i) ->
	  begin
	    match jbt with
	      | `Long ->
		  if (i >=0 && i<=3) then "lload_"^(string_of_int i)
		  else "lload "^(string_of_int i)
	      | `Float ->
		  if (i >=0 && i<=2) then "fload_"^(string_of_int i)
		  else "fload "^(string_of_int i)
	      | `Double ->
		  if (i >=0 && i<=3) then "dload_"^(string_of_int i)
		  else "dload "^(string_of_int i)
	      | _ ->
		  if (i >=0 && i<=3) then "iload_"^(string_of_int i)
		  else "iload "^(string_of_int i)
	  end
      | OpALoad(i)->
	  if (i >=0 && i<=3) then "aload_"^(string_of_int i)
	  else "aload "^(string_of_int i)

      (*	| OpArrayLoad of [`Int | other_num]  -> ""*)
      | OpAALoad  -> "aaload"
      | OpBALoad  -> "baload"
      | OpCALoad  -> "caload"
      | OpSALoad  -> "saload"

      | OpStore(jbt,i) ->
	  begin
	    match jbt with
	      | `Long ->
		  if (i >=0 && i<=3) then "lstore_"^(string_of_int i)
		  else "lstore "^(string_of_int i)
	      | `Float ->
		  if (i >=0 && i<=3) then "fstore_"^(string_of_int i)
		  else "fstore "^(string_of_int i)
	      | `Double ->
		  if (i >=0 && i<=3) then "dstore_"^(string_of_int i)
		  else "dstore "^(string_of_int i)
	      | _ ->
		  if (i >=0 && i<=3) then "istore_"^(string_of_int i)
		  else "istore "^(string_of_int i)
	  end
      | OpAStore(i)  ->
	  if (i >=0 && i<=3) then "astore_"^(string_of_int i)
	  else "astore "^(string_of_int i)

      (*	| OpArrayStore of [`Int | other_num]  -> "" *)
      | OpAAStore  -> "aastore"
      | OpBAStore  -> "bastore"
      | OpCAStore  -> "castore"
      | OpSAStore  -> "sastore"

      | OpPop  -> "pop"
      | OpPop2  -> "pop2"
      | OpDup  -> "dup"
      | OpDupX1  -> "dup_x1"
      | OpDupX2  -> "dup_x2"
      | OpDup2  -> "dup2"
      | OpDup2X1  -> "dup2_x1"
      | OpDup2X2  -> "dup2_x2"
      | OpSwap  -> "swap"

      | OpAdd(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"add"
      | OpSub(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"sub"
      | OpMult(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"mult"
      | OpDiv(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"div"
      | OpRem(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"rem"
      | OpNeg(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"neg"

      | OpIShl  -> "ishl"
      | OpLShl  -> "lshl"
      | OpIShr  -> "ishr"
      | OpLShr  -> "lshr"
      | OpIUShr  -> "iushr"
      | OpLUShr  -> "lushr"
      | OpIAnd  -> "iand"
      | OpLAnd  -> "land"
      | OpIOr  -> "ior"
      | OpLOr  -> "lor"
      | OpIXor  -> "ixor"
      | OpLXor  -> "ilor"

      | OpIInc(i,j) -> "iinc "^(string_of_int i)^(" ")^(string_of_int j)

      | OpI2L -> "i2l"
      | OpI2F -> "i2f"
      | OpI2D -> "i2d"
      | OpL2I -> "l2i"
      | OpL2F -> "l2f"
      | OpL2D -> "l2d"
      | OpF2I -> "f2i"
      | OpF2L -> "f2l"
      | OpF2D -> "f2d"
      | OpD2I -> "d2i"
      | OpD2L -> "d2l"
      | OpD2F -> "d2f"
      | OpI2B -> "i2b"
      | OpI2C -> "i2c"
      | OpI2S -> "i2s"

      | OpLCmp -> "lcmp"
      | OpFCmpL -> "fcmpl"
      | OpFCmpG -> "fcmpg"
      | OpDCmpL -> "dcmpl"
      | OpDCmpG -> "dcmpg"
      | OpIfEq(i) -> "ifeq "^(string_of_int (i+pos))
      | OpIfNe(i) -> "ifne "^(string_of_int (i+pos))
      | OpIfLt(i) -> "iflt "^(string_of_int (i+pos))
      | OpIfGe(i) -> "ifge "^(string_of_int (i+pos))
      | OpIfGt(i) -> "ifgt "^(string_of_int (i+pos))
      | OpIfLe(i) -> "ifle "^(string_of_int (i+pos))
      | OpICmpEq(i) -> "if_icmpeq "^(string_of_int (i+pos))
      | OpICmpNe(i) -> "if_icmpne "^(string_of_int (i+pos))
      | OpICmpLt(i) -> "if_icmplt "^(string_of_int (i+pos))
      | OpICmpGe(i) -> "if_icmpge "^(string_of_int (i+pos))
      | OpICmpGt(i) -> "if_icmpgt "^(string_of_int (i+pos))
      | OpICmpLe(i) -> "if_icmple "^(string_of_int (i+pos))
      | OpACmpEq(i) -> "if_acmpeq "^(string_of_int (i+pos))
      | OpACmpNe(i) -> "if_acmpne "^(string_of_int (i+pos))
      | OpGoto(i) -> "goto "^(string_of_int (i+pos))
      | OpJsr(i) -> "jsr "^(string_of_int (i+pos))
      | OpRet(i) -> "ret "^(string_of_int i)

      | OpTableSwitch(d,l,_,jumps) ->
	  "tableswitch "^(Int32.to_string l)^"\n"^
	    (String.concat "\n" (List.map
				   (fun x -> "\t\t\t\t\t"^
				      (string_of_int (x)))
				   (Array.to_list jumps)))^"\n"^
	    "\t\t\tdefault : \t"^(string_of_int (d))
      | OpLookupSwitch(d,jumps) ->
	  "lookupswitch \n"^
	    (String.concat "\n" (List.map
				   (fun (x,y) -> "\t\t\t"^
				      (Int32.to_string x )^" : \t\t"^
				      (string_of_int (y)))
				   jumps))^"\n"^
	    "\t\t\tdefault : \t"^(string_of_int (d))

      | OpReturn(jvmbt) -> Char.escaped (JDumpBasics.jvm_basic_type jvmbt)^"return"
      | OpAReturn -> "areturn"
      | OpReturnVoid -> "return"

      | OpGetStatic(i) -> "getstatic "^(string_of_constant constants.(i))
      | OpPutStatic(i) -> "putstatic "^(string_of_constant constants.(i))
      | OpGetField(i) -> "getfield "^(string_of_constant constants.(i))
      | OpPutField(i) -> "putfield "^(string_of_constant constants.(i))
      | OpInvokeVirtual(i) -> "invokevirtual "^(string_of_constant constants.(i))
      | OpInvokeNonVirtual(i) -> "invokenonvirtual "^(string_of_constant constants.(i))
      | OpInvokeStatic(i) -> "invokestatic "^(string_of_constant constants.(i))
      | OpInvokeInterface(i,c) -> "invokeinterface "^(string_of_constant constants.(i))^" "^(string_of_int c)

      | OpNew(i) -> "new "^(string_of_constant constants.(i))
      | OpNewArray(jbt) -> "newarray "^(JDumpBasics.basic_type jbt)
      | OpANewArray(i)-> "anewarray "^(string_of_constant constants.(i))
      | OpArrayLength -> "arraylength"
      | OpThrow -> "athrow"
      | OpCheckCast(i) -> "checkcast "^(string_of_constant constants.(i))
      | OpInstanceOf(i) -> "instanceof "^(string_of_constant constants.(i))
      | OpMonitorEnter -> "monitorenter"
      | OpMonitorExit -> "monitorexit"
      | OpAMultiNewArray(i,j) -> "multianewarray "^
	  (string_of_constant constants.(i))^(" ")^(string_of_int j)
      | OpIfNull(i) -> "ifnull "^(string_of_int (i+pos))
      | OpIfNonNull(i) -> "ifnonnull "^(string_of_int (i+pos))
      | OpGotoW(i) -> "goto_w "^(string_of_int (i+pos))
      | OpJsrW(i) -> "jsr_w "^(string_of_int (i+pos))
      | OpBreakpoint -> "breakpoint"
      (* | OpRetW(i) -> "retw "^(string_of_int (i+pos)) *)
      | OpInvalid -> "Invalid"
      | OpArrayStore ar_t -> (Char.escaped (JDumpBasics.jvm_basic_type ar_t))^"astore"
      | OpArrayLoad ar_t -> (Char.escaped (JDumpBasics.jvm_basic_type ar_t))^"aload"


(* Extract Attributes *)

let rec find_Constant = function
  | AttributeConstant c::_ -> "= "^(string_of_constant_value c)
  | _::l -> find_Constant l
  | [] -> ""

let rec find_SourceFile = function
  | AttributeSourceFile s::_ -> s
  | _::l -> find_SourceFile l
  | [] -> "UnknownSourceFile"

let find_Code m =
  let rec f = function
    | AttributeCode c::_ -> Some (Lazy.force c)
    | _::l -> f l
    | [] -> None
  in f m.m_attributes

let string_of_attribute a = match a with
  | AttributeStackMap _ -> "stackmap"
  | AttributeLineNumberTable _ -> "linenumbertable"
  | AttributeDeprecated -> "deprecated"
  | AttributeUnknown(s,s') -> s^" "^(string_of_int (String.length s'))
  | _ -> "Unkown attribute"

(*let find_stack_map x =
  let rec f = function
    | AttributeStackMapTable sm::_ -> sm
    | _::l ->f l
    | [] -> []
  in  f x*)

let rec find_lines x = match x with
  | AttributeLineNumberTable lines::_ -> lines
  | _::l -> find_lines l
  | [] -> []

let rec find_vars x = match x with
  | AttributeLocalVariableTable vars::_ -> vars
  | _::l -> find_vars l
  | [] -> []

let get_line attributes i =
  try
    Some (List.assoc i (find_lines attributes))
  with _ -> None

(*let string_of_vtype _consts vtype = match vtype with
  | VTop -> "Top"
  | VInteger -> "Integer"
  | VFloat -> "Float"
  | VDouble -> "Double"
  | VLong -> "Long"
  | VNull -> "Null"
  | VUninitializedThis -> "UninitializedThis"
  | VObject(i) -> "Object "^(string_objtype_2_classname i)
  | VUninitialized(_i) -> "Uninitialized"

let string_of_stack_map_frame consts sm last = match sm with
  | SameFrame(k) ->
      let offset = !last + k + 1 in
	last := offset;
	( "\t.stack use locals ; same_frame("^(string_of_int k)^")\n"^
	    "\t\toffset "^(string_of_int offset)^"\n"^
	    "\t.end stack\n")
  | SameLocals(k,vtype) ->
      let offset = !last + k - 64 + 1 in
	last := offset;
	( "\t.stack use locals ; same_locals_1_stack_item_frame("^
	    (string_of_int k)^","^(string_of_vtype consts vtype)^")\n"^
	    "\t\toffset "^(string_of_int offset)^"\n"^
	    "\t\tstack "^(string_of_vtype consts vtype)^"\n"^
	    "\t.end stack\n")
  | SameLocalsExtended(k,i,vtype) ->
      let offset = !last + i + 1 in
	last := offset;
	( "\t.stack use locals ; same_locals_1_stack_item_frame_extended("^
	    (string_of_int k)^","^(string_of_int i)^","^(string_of_vtype consts vtype)^")\n"^
	    "\t\toffset "^(string_of_int offset)^"\n"^
	    "\t\tstack "^(string_of_vtype consts vtype)^"\n"^
	    "\t.end stack\n")
  | ChopFrame(k,i) ->
      let offset = !last + i + 1 in
	last := offset;
	( "\t.stack use locals ; chop_frame("^
	    (string_of_int k)^","^(string_of_int i)^")\n"^
	    "\t\toffset "^(string_of_int offset)^"\n"^
	    "\t.end stack\n")
  | SameFrameExtended(k,i) ->
      let offset = !last + i + 1 in
	last := offset;
	( "\t.stack use locals ; same_frame_extended("^
	    (string_of_int k)^","^(string_of_int i)^")\n"^
	    "\t\toffset "^(string_of_int offset)^"\n"^
	    "\t.end stack")
  | AppendFrame(k,i,vtypes) ->
      let offset = !last + i + 1 in
	last := offset;
	let locals =
	  String.concat "\n" (List.map (fun x -> "\t\tlocals "^
					  (string_of_vtype consts x)) vtypes) in
	  ( "\t.stack use locals ; append_frame("^
	      (string_of_int k)^","^(string_of_int i)^",locals)\n"^
	      "\t\toffset "^(string_of_int offset)^"\n"^
	      locals^"\n"^
	      "\t.end stack\n")
  | FullFrame(k,i,vtypes,vtypes') ->
      let offset = !last + i + 1 in
	last := offset;
	let locals =
	  String.concat "\n" (List.map (fun x -> "\t\tlocals "^(string_of_vtype consts x)) vtypes)
	and stack =
	  String.concat "\n" (List.map (fun x -> "\t\tstack "^(string_of_vtype consts x)) vtypes')
	in
	  ( "\t.stack use locals ; full_frame("^
	      (string_of_int k)^","^(string_of_int i)^",locals,stack)\n"^
	      "\t\toffset "^(string_of_int offset)^"\n"^
	      locals^"\n"^
	      "\t\t"^stack^"\n"^
	      "\t.end stack\n")*)

let dump ch cl =
  let version = string_of_version cl.j_version
  in let (source:string) =
      let rec f = function
	| AttributeSourceFile s::_ -> s | _::l -> f l | [] -> ""
      in f cl.j_attributes
  in let class_flags = string_of_class_flags cl.j_flags
  in let class_name = string_of_classname cl.j_name
  in let super = match cl.j_super with
    | Some cn -> JDumpBasics.class_name cn
    | _ -> "java.lang.Object"
  in let interfaces = List.map string_of_classname cl.j_interfaces
  in
    begin
      IO.printf ch ".bytecode %s\n" version;
      IO.printf ch ".source %s\n\n" source;
      IO.printf ch ".class %s %s\n" class_flags class_name;
      IO.printf ch ".super %s\n" super;
      List.iter (fun str -> IO.printf ch ".implements %s\n" str) interfaces;
      IO.printf ch "%s" "\n";
      List.iter
	(fun field ->
	   let field_flags = string_of_field_flags field.f_flags
	   in let field_desc = string_of_value_type field.f_descriptor
	   in let constant =
	       let rec f = function
		 | AttributeConstant c::_ -> "= "^(string_of_constant_value c)
		 | _::l -> f l | [] -> ""
	       in f field.f_attributes
	   in IO.printf ch ".field %s %s %s %s\n" field_flags field.f_name field_desc constant
	) cl.j_fields;
      IO.printf ch "%s" "\n";
      List.iter
	(fun meth ->
	   let meth_flags = string_of_method_flags meth.m_flags
	   in let meth_desc = string_of_method_descriptor meth.m_descriptor in
	     begin
	       IO.printf ch ".method %s %s%s\n" meth_flags meth.m_name meth_desc;
	       begin
		 match find_Code meth with
		   | Some code ->
		       (*let last = ref (-1) in
			 (List.iter
			    (fun x -> IO.printf ch "%s"
			       (string_of_stack_map_frame cl.j_consts x last))
			    (find_stack_map (code.c_attributes)));*)
			 IO.printf ch "\t.limit stack %s\n" (string_of_int code.c_max_stack);
			 IO.printf ch "\t.limit locals %s\n" (string_of_int code.c_max_locals);
			 List.iter (fun (start,length,name,typ,index) ->
				      IO.printf ch "\t.var %s is %s %s from %s to %s\n"
					(string_of_int index) name (string_of_value_type typ)
					(string_of_int start) (string_of_int (start+length))
				   )
			   (find_vars code.c_attributes);
			 for i = 0 to Array.length code.c_code-1 do
			   let _ =
			     match get_line code.c_attributes i with
			       | Some i -> IO.printf ch ("\t.line %s\n") (string_of_int i)
			       | None -> ()
			   in
			   let _ =
			     match string_of_opcode code.c_code.(i) cl.j_consts i with
			       | "Invalid" -> ()
			       | str -> IO.printf ch "\t\t%s: \t%s\n" (string_of_int i) str
			   in ()
			 done
		   | None -> ()
	       end;
	       IO.printf ch ".end method\n";
	       IO.printf ch "%s" "\n";
	     end)
	cl.j_methods
    end
