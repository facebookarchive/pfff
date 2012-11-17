(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008 Laurent Hubert (CNRS)
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

(** Low level representation of a Java class file. *)

open JBasics

(** {2 Low level bytecode instructions.} *)

(** Instruction. *)
type opcode =
  | OpNop
  | OpAConstNull
  | OpIConst of int32
  | OpLConst of int64
  | OpFConst of float
  | OpDConst of float
  | OpBIPush of int
  | OpSIPush of int
  | OpLdc1 of int
  | OpLdc1w of int
  | OpLdc2w of int

  | OpLoad of jvm_basic_type * int
  | OpALoad of int

  | OpArrayLoad of [`Int | other_num]
  | OpAALoad
  | OpBALoad
  | OpCALoad
  | OpSALoad

  | OpStore of jvm_basic_type * int
  | OpAStore of int

  | OpArrayStore of [`Int | other_num]
  | OpAAStore
  | OpBAStore
  | OpCAStore
  | OpSAStore

  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  | OpIShl
  | OpLShl
  | OpIShr
  | OpLShr
  | OpIUShr
  | OpLUShr
  | OpIAnd
  | OpLAnd
  | OpIOr
  | OpLOr
  | OpIXor
  | OpLXor

  | OpIInc of int * int (** index, increment *)

  | OpI2L
  | OpI2F
  | OpI2D
  | OpL2I
  | OpL2F
  | OpL2D
  | OpF2I
  | OpF2L
  | OpF2D
  | OpD2I
  | OpD2L
  | OpD2F
  | OpI2B
  | OpI2C
  | OpI2S

  | OpLCmp
  | OpFCmpL
  | OpFCmpG
  | OpDCmpL
  | OpDCmpG
  | OpIfEq of int
  | OpIfNe of int
  | OpIfLt of int
  | OpIfGe of int
  | OpIfGt of int
  | OpIfLe of int
  | OpICmpEq of int
  | OpICmpNe of int
  | OpICmpLt of int
  | OpICmpGe of int
  | OpICmpGt of int
  | OpICmpLe of int
  | OpACmpEq of int
  | OpACmpNe of int
  | OpGoto of int
  | OpJsr of int
  | OpRet of int

  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  | OpReturn of jvm_basic_type
  | OpAReturn
  | OpReturnVoid

  | OpGetStatic of int
  | OpPutStatic of int
  | OpGetField of int
  | OpPutField of int
  | OpInvokeVirtual of int
  | OpInvokeNonVirtual of int
  | OpInvokeStatic of int
  | OpInvokeInterface of int * int (** count *)

  | OpNew of int
  | OpNewArray of java_basic_type
  | OpANewArray of int
  | OpArrayLength
  | OpThrow
  | OpCheckCast of int
  | OpInstanceOf of int
  | OpMonitorEnter
  | OpMonitorExit
  | OpAMultiNewArray of int * int (** ClassInfo, dims *)
  | OpIfNull of int               (* offset *)
  | OpIfNonNull of int            (* offset *)
  | OpGotoW of int                (* offset *)
  | OpJsrW of int                 (* offset *)
  | OpBreakpoint                  (* should not be found *)
  (* | OpRetW of int *)
  | OpInvalid
      (* if [opcodes.(i) = OpInvalid] it means that there is an opcode
         that starts at position j, with j<i, an covers positions up
         to k, with k>=i. *)

type opcodes = opcode array

(** {2 Flags, attributes and low-level structure of class files.} *)

type common_flag = [
| `AccPublic
| `AccSynthetic
| `AccRFU of int (** The int is a mask. *)
| `AccFinal
]

type inner_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccInterface
| `AccAbstract
| `AccAnnotation
| `AccEnum
]

type field_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccVolatile
| `AccTransient
| `AccEnum
]

type method_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccSynchronized
| `AccBridge
| `AccVarArgs
| `AccNative
| `AccAbstract
| `AccStrict
]

type class_flag = [
| common_flag
| `AccAbstract
| `AccAnnotation
| `AccEnum
| `AccInterface
| `AccSuper
]

type access_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccSynchronized
| `AccVolatile
| `AccTransient
| `AccNative
| `AccInterface
| `AccAbstract
| `AccStrict
| `AccSuper
| `AccBridge
| `AccVarArgs
| `AccAnnotation
| `AccEnum
]


(** DFr : Addition for 1.6 stackmap. *)
type stackmap_frame =
  | SameFrame of int
  | SameLocals of int * verification_type
  | SameLocalsExtended of int * int * verification_type
  | ChopFrame of int * int
  | SameFrameExtended of int * int
  | AppendFrame of int * int * verification_type list
  | FullFrame of int * int * verification_type list * verification_type list

type code = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : opcodes;
  c_exc_tbl : JCode.exception_handler list;
  c_attributes : attribute list;
}

and attribute =
  | AttributeSourceFile of string
  | AttributeConstant of constant_value
  | AttributeCode of code Lazy.t
  | AttributeExceptions of class_name list
  | AttributeInnerClasses of
      (class_name option * class_name option * string option
       * inner_flag list) list
	(** inner_class_info, outer_class_info, inner_name,
	    inner_class_access_flags *)
  | AttributeSynthetic
  | AttributeLineNumberTable of (int * int) list
  | AttributeLocalVariableTable of (int * int * string * value_type * int) list
      (** start_pc, length, name, type, index *)
  | AttributeLocalVariableTypeTable of (int * int * string * JSignature.fieldTypeSignature * int) list
      (** (start_pc, length, name, type, index), LocalVariableTable for
          generics, described in the JVM Spec 1.5, §4.8.13 *)
  | AttributeDeprecated
  | AttributeStackMap of (int*(verification_type list)
			  *(verification_type list)) list
  | AttributeSignature of string
      (** Introduced in Java 5 for generics
	  ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}).*)
  | AttributeEnclosingMethod of (class_name * (string * descriptor) option)
      (** Introduced in Java 5 for local classes (classes
	  defined in a method body)
	  ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}).*)
  | AttributeSourceDebugExtension of string
      (** Introduced in Java 5 for debugging purpose (no
	  semantics defined)
	  ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  | AttributeStackMapTable of stackmap_frame list
      (** DFr : Addition for 1.6 stackmap. *)
  | AttributeRuntimeVisibleAnnotations of annotation list
  | AttributeRuntimeInvisibleAnnotations of annotation list
  | AttributeRuntimeVisibleParameterAnnotations of annotation list list
  | AttributeRuntimeInvisibleParameterAnnotations of annotation list list
  | AttributeAnnotationDefault of element_value  (* cf. §4.8.19 of JVM Spec 5 *)
  | AttributeUnknown of string * string

type jfield = {
  f_name : string;
  f_descriptor : value_type;
  f_flags : field_flag list;
  f_attributes : attribute list
}

type jmethod = {
  m_name : string;
  m_descriptor : method_descriptor;
  m_flags : method_flag list;
  m_attributes : attribute list
}

type jclass = {
  j_name : class_name;
  j_super : class_name option;
  j_interfaces : class_name list;
  j_consts : constant array;
  j_flags : class_flag list;
  j_fields : jfield list;
  j_methods : jmethod list;
  j_attributes : attribute list;
  j_version : version;
}
