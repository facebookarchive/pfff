(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
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

open JBasics
open JClassLow
open JDumpBasics

let sprintf = Printf.sprintf

let opcode = function
  | OpNop -> "nop"
  | OpAConstNull -> "aconstnull"
  | OpIConst i -> sprintf "iconst %ld" i
  | OpLConst i -> sprintf "lconst %Ld" i
  | OpFConst f -> sprintf "fconst %f" f
  | OpDConst f -> sprintf "dconst %f" f
  | OpBIPush i -> sprintf "biconst %d" i
  | OpSIPush i -> sprintf "siconst %d" i
  | OpLdc1 i -> sprintf "ldc1 %d" i
  | OpLdc1w i -> sprintf "ldc1w %d" i
  | OpLdc2w i -> sprintf "ldc2w %d" i

  | OpLoad (t,i) -> sprintf "%cload %d" (jvm_basic_type t) i
  | OpALoad i -> sprintf "aload %d" i

  | OpArrayLoad t -> sprintf "%caload" (jvm_basic_type t)
  | OpAALoad -> "aaload"
  | OpBALoad -> "baload"
  | OpCALoad -> "caload"
  | OpSALoad -> "saload"

  | OpStore (t,i) -> sprintf "%cstore %d" (jvm_basic_type t) i
  | OpAStore i -> sprintf "astore %d" i

  | OpArrayStore t -> sprintf "%castore" (jvm_basic_type t)
  | OpAAStore -> "aastore"
  | OpBAStore -> "bastore"
  | OpCAStore -> "castore"
  | OpSAStore -> "sastore"

  | OpPop -> "pop"
  | OpPop2 -> "pop2"
  | OpDup -> "dup"
  | OpDupX1 -> "dupX1"
  | OpDupX2 -> "dupX2"
  | OpDup2 -> "dup2"
  | OpDup2X1 -> "dup2X1"
  | OpDup2X2 -> "dup2X2"
  | OpSwap -> "swap"

  | OpAdd k -> sprintf "%cadd" (jvm_basic_type k)
  | OpSub k -> sprintf "%csub" (jvm_basic_type k)
  | OpMult k -> sprintf "%cmult" (jvm_basic_type k)
  | OpDiv k -> sprintf "%cdiv" (jvm_basic_type k)
  | OpRem k -> sprintf "%crem" (jvm_basic_type k)
  | OpNeg k -> sprintf "%cneg" (jvm_basic_type k)

  | OpIShl -> "ishl"
  | OpLShl -> "lshl"
  | OpIShr -> "ishr"
  | OpLShr -> "lshr"
  | OpIUShr -> "iushr"
  | OpLUShr -> "lushr"
  | OpIAnd -> "iand"
  | OpLAnd -> "land"
  | OpIOr -> "ior"
  | OpLOr -> "lor"
  | OpIXor -> "ixor"
  | OpLXor -> "lxor"

  | OpIInc (a,b) -> sprintf "iinc %d %d" a b

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
  | OpIfEq i -> sprintf "ifEq %d" i
  | OpIfNe i -> sprintf "ifNe %d" i
  | OpIfLt i -> sprintf "ifLt %d" i
  | OpIfGe i -> sprintf "ifGe %d" i
  | OpIfGt i -> sprintf "ifGt %d" i
  | OpIfLe i -> sprintf "ifLe %d" i
  | OpICmpEq i -> sprintf "ifcmpeq %d" i
  | OpICmpNe i -> sprintf "ifcmpne %d" i
  | OpICmpLt i -> sprintf "ifcmplt %d" i
  | OpICmpGe i -> sprintf "ifcmpge %d" i
  | OpICmpGt i -> sprintf "ifcmpgt %d" i
  | OpICmpLe i -> sprintf "ifcmple %d" i
  | OpACmpEq i -> sprintf "ifacmpeq %d" i
  | OpACmpNe i -> sprintf "ifacmpne %d" i
  | OpGoto i -> sprintf "goto %d" i
  | OpJsr i -> sprintf "jsr %d" i
  | OpGotoW i -> sprintf "gotow %d" i
  | OpJsrW i -> sprintf "jsrw %d" i
  | OpRet i -> sprintf "ret %d" i

  | OpTableSwitch (def,min,max,tbl) ->
      (* "tableswitch ([_:_] -> [_,_,_,...],default:_)" *)
      let inst = "tableswitch (["^ Int32.to_string min ^":"^ Int32.to_string max ^"] -> ["
      and table = String.concat "," (Array.to_list (Array.map string_of_int tbl))
      in inst^table^"],default:"^ string_of_int def^")"

  | OpLookupSwitch (default,jumps) ->
      let inst =
	List.fold_left
	  (fun s (int,offset) -> s ^ Int32.to_string int ^"->" ^ string_of_int offset^ " | ")
	  "lookupswitch "
	  jumps
      in inst ^ "_ ->" ^string_of_int default

  | OpReturn k -> sprintf "%creturn" (jvm_basic_type k)
  | OpAReturn -> "areturn"
  | OpReturnVoid -> "return"

  | OpGetStatic i -> sprintf "getstatic %d" i
  | OpPutStatic i -> sprintf "putstatic %d" i
  | OpGetField i -> sprintf "getfield %d" i
  | OpPutField i -> sprintf "putfield %d" i
  | OpInvokeVirtual i -> sprintf "invokevirtual %d" i
  | OpInvokeNonVirtual i -> sprintf "invokespecial %d" i
  | OpInvokeStatic i -> sprintf "invokestatic %d" i
  | OpInvokeInterface (i,n) -> sprintf "invokeinterface %d %d" i n

  | OpNew i -> sprintf "new %d" i
  | OpNewArray k -> sprintf "%cnewarray" (java_basic_type k)
  | OpANewArray i -> sprintf "anewarray %d" i
  | OpArrayLength -> "arraylenth"
  | OpThrow -> "throw"
  | OpCheckCast i -> sprintf "checkcast %d" i
  | OpInstanceOf i -> sprintf "instanceof %d" i
  | OpMonitorEnter -> "monitorenter"
  | OpMonitorExit -> "monitorexit"
  | OpAMultiNewArray (c,n) -> sprintf "amultinewarray type:%d dims:%d" c n
  | OpIfNull i -> sprintf "ifnull %d" i
  | OpIfNonNull i -> sprintf "ifnonnull %d" i
  | OpBreakpoint -> "breakpoint"

  | OpInvalid -> "invalid"


let access_flags = function
  | [] -> ""
  | flags ->
      String.concat " "
        (List.map
           (function
	      | `AccPublic -> "public"
	      | `AccPrivate -> "private"
	      | `AccProtected -> "protected"
	      | `AccStatic -> "static"
	      | `AccFinal -> "final"
	      | `AccSynchronized -> "synchronized"
	      | `AccVolatile -> "volatile"
	      | `AccTransient -> "transient"
	      | `AccNative -> "native"
	      | `AccInterface -> "interface"
	      | `AccAbstract -> "abstract"
	      | `AccStrict -> "strict"
	      | `AccEnum -> "enum"
	      | `AccAnnotation -> "annotation"
	      | `AccVarArgs -> "VarArgs"
	      | `AccBridge -> "bridge"
	      | `AccSuper -> "`AccSuper"
	      | `AccSynthetic -> "synthetic"
	      | `AccRFU i -> Printf.sprintf "rfu 0x%X" i
	   ) flags) ^ " "

let dump_java6_stackmap ch frame =
  match frame with
    | SameFrame k -> IO.printf ch "SameFrame(tag:%d)\n" k
    | SameLocals (k,vtype) ->
	IO.printf ch "SameLocals(tag:%d,%s)\n" k (dump_verification_type vtype)
    | SameLocalsExtended (k,i,vtype) ->
	IO.printf ch "SameLocalsExtended(tag:%d,%d,%s)\n"
	  k i (dump_verification_type vtype)
    | ChopFrame (k,i) ->
	IO.printf ch "ChopFrame(tag:%d,%d)\n" k i
    | SameFrameExtended (k,i) ->
	IO.printf ch "SameFrameExtended(tag:%d,%d)\n" k i
    | AppendFrame (k,i,vtypes) ->
	let svtypes = String.concat "," (List.map dump_verification_type vtypes) in
	  IO.printf ch "AppendFrame(tag:%d,%d,%s)\n" k i svtypes
    | FullFrame (k,offset,locals,stack) ->
	IO.printf ch "FullFrame(tag:%d," k;
	dump_stackmap ch (offset,locals,stack);
	IO.nwrite ch ")\n"

let dump_inner_classes ch icl =
  List.iter
    (fun (ic,oc,icsource,iflags) ->
       IO.nwrite ch "\n      ";
       (* flags oc.ic (named icsource at source) *)
       IO.nwrite ch (access_flags iflags);
       IO.write ch ' ';
       (match oc with
          | None -> ()
          | Some ocname -> 
              IO.nwrite ch (cn_name ocname);
              IO.write ch '.');
       (match ic with
          | None -> IO.write ch '_'
          | Some icname -> IO.nwrite ch (cn_name icname));
       (match icsource with
          | None -> IO.nwrite ch " (anonymous in source)"
          | Some icname ->
              IO.nwrite ch (" (named "^icname^" in the source)"));
    )
    icl

let dump_list sep print = function
  | [] -> ()
  | hd::tl ->
      print hd;
      List.iter
        (fun item -> sep (); print item)
        tl

let rec dump_element_value ch = function
  | EVCstByte cst ->
      IO.nwrite ch (string_of_int cst)
  | EVCstChar cst ->
      IO.nwrite ch "char ";
      IO.nwrite ch (string_of_int cst)
  | EVCstInt cst ->
      IO.nwrite ch (Int32.to_string cst)
  | EVCstShort cst ->
      IO.nwrite ch (string_of_int cst)
  | EVCstBoolean cst ->
      IO.nwrite ch (if cst = 0 then "false" else "true");
  | EVCstDouble cst ->
      IO.nwrite ch (string_of_float cst)
  | EVCstFloat cst ->
      IO.nwrite ch (string_of_float cst);
      IO.nwrite ch "f"      
  | EVCstLong cst ->
      IO.nwrite ch (Int64.to_string cst);
      IO.nwrite ch "L"
  | EVCstString cst ->
      IO.nwrite ch "\"";
      for i = 0 to (String.length cst) - 1 do
        match cst.[i] with
          | '\\' -> IO.nwrite ch "\\\\"  (* writes 2 slashes (but escaped) *)
          | '"' -> IO.nwrite ch "\\\""  (* writes backslash and doublequote (but escaped) *)
          | c -> IO.write ch c
      done;
      IO.nwrite ch "\""
  | EVEnum (s1,s2) ->
      IO.nwrite ch (cn_name s1 ^ "." ^ s2)
  | EVClass None ->
      IO.nwrite ch "Void.class"
  | EVClass (Some vt) ->
      IO.nwrite ch (value_signature ~jvm:true vt)
  | EVAnnotation annot ->
      dump_annotation ch annot
  | EVArray evlist ->
      IO.nwrite ch "[|";
      dump_list (fun () -> IO.nwrite ch "; ") (dump_element_value ch) evlist;
      IO.nwrite ch "|]"

and dump_annotation ch annot =
  let dump_pair (s,ev) =
    IO.nwrite ch s;
    IO.nwrite ch " = ";
    dump_element_value ch ev
  in
  let dump_pairs pairs =
    dump_list (fun () -> IO.nwrite ch ", ") dump_pair pairs
  in
    IO.write ch '@';
    IO.nwrite ch (JBasics.cn_name annot.kind);
    IO.write ch '(';
    dump_pairs annot.element_value_pairs;
    IO.write ch ')'

let rec dump_code ch consts code =
  IO.nwrite ch "    max_stack = ";
  IO.nwrite ch (string_of_int code.JClassLow.c_max_stack);
  IO.nwrite ch " , max_locals = ";
  IO.nwrite ch (string_of_int code.JClassLow.c_max_locals);
  IO.write ch '\n';
  Array.iteri (fun i c ->
		 match c with
		   | OpInvalid -> (); (* IO.printf ch "__\n" *)
		   | _ -> IO.printf ch "      %.4i (%.4X) %s\n" i i (opcode c)
	      ) code.JClassLow.c_code;
  if code.JClassLow.c_exc_tbl <> []
  then
    begin
      IO.nwrite ch "    exceptions";
      List.iter (dump_exc ch consts) code.JClassLow.c_exc_tbl;
      IO.write ch '\n';
    end;
  List.iter (dump_attrib ch consts) code.JClassLow.c_attributes

and dump_attrib ch consts = function
  | AttributeSourceFile s ->
      IO.printf ch "    SourceFile : \"%s\"\n" s
  | AttributeSignature s ->
      IO.printf ch "    signature = %s\n" s
  | AttributeEnclosingMethod (cn,mso) ->
      IO.nwrite ch "    enclosing method : class = ";
      IO.nwrite ch (JDumpBasics.class_name cn);
      IO.nwrite ch ", method = ";
      (match mso with
	 | None -> IO.nwrite ch "None"
	 | Some (mn,ms) -> IO.nwrite ch (JDumpBasics.signature mn ms));
      IO.write ch '\n'
  | AttributeSourceDebugExtension s ->
      IO.nwrite ch ("    SourceDebugExtension = "^s^"\n")
  | AttributeConstant c ->
      IO.nwrite ch "    const ";
      dump_constant_value ch c;
      IO.write ch '\n';
  | AttributeCode code ->
      dump_code ch consts (Lazy.force code) (* IO.printf ch "    unexpected code attribute" *)
  | AttributeExceptions l ->
      IO.nwrite ch "    exceptions";
      List.iter (fun cn -> IO.nwrite ch (class_name cn^" ")) l;
      IO.write ch '\n'
  | AttributeInnerClasses icl ->
      IO.nwrite ch "    inner-classes:";
      dump_inner_classes ch icl;
      IO.write ch '\n'
  | AttributeSynthetic ->
      IO.printf ch "    synthetic\n"
  | AttributeLineNumberTable lines ->
      IO.printf ch "    LineNumberTable";
      List.iter
        (fun (i1,i2) ->
           IO.nwrite ch ("\n      " ^string_of_int i1 ^ ":" ^string_of_int i2))
        lines;
      IO.write ch '\n'
  | AttributeLocalVariableTable variables ->
      IO.nwrite ch "    local-variables\n";
      List.iter
	(function start_pc, length, name, signature, index ->
	   IO.printf ch "      from %d to %d, %s %s at %d\n"
	     start_pc
	     (start_pc + length)
	     (value_signature signature)
	     name
	     index)
	variables
  | AttributeLocalVariableTypeTable variables ->
      IO.nwrite ch "    generic-local-variables\n";
      List.iter
	(function start_pc, length, name, signature, index ->
	   IO.printf ch "      from %d to %d, %s %s at %d\n"
	     start_pc
	     (start_pc + length)
	     (JUnparseSignature.unparse_FieldTypeSignature signature)
	     name
	     index)
	variables
  | AttributeDeprecated ->
      IO.nwrite ch "    deprecated\n"
  | AttributeStackMap stackmap_frames ->
      IO.nwrite ch "    stackmap midp = [";
      List.iter (dump_stackmap ch) stackmap_frames;
      IO.nwrite ch "]\n"
  | AttributeStackMapTable stackmap_frames ->
      IO.nwrite ch "    stackmap java6 = [";
      List.iter (dump_java6_stackmap ch) stackmap_frames;
      IO.nwrite ch "]\n"
  | AttributeAnnotationDefault a ->
      IO.nwrite ch "    AnnotationDefault = ";
      dump_element_value ch a;
      IO.write ch '\n'
  | AttributeRuntimeVisibleAnnotations al ->
      IO.nwrite ch "    RuntimeVisibleAnnotations";
      List.iter
        (fun a ->
           IO.nwrite ch "\n        ";
           dump_annotation ch a)
        al;
      IO.write ch '\n'
  | AttributeRuntimeInvisibleAnnotations al ->
      IO.nwrite ch "    RuntimeInvisibleAnnotations";
      List.iter
        (fun a ->
           IO.nwrite ch "\n      ";
           dump_annotation ch a)
        al;
      IO.write ch '\n'
  | AttributeRuntimeInvisibleParameterAnnotations all ->
      IO.nwrite ch "    RuntimeInvisibleParameterAnnotations = ";
      List.iter
        (fun al -> 
           IO.nwrite ch "\n      [";
           dump_list (fun () -> IO.nwrite ch ", ") (dump_annotation ch) al;
           IO.write ch ']')
        all;
      IO.write ch '\n'
  | AttributeRuntimeVisibleParameterAnnotations all ->
      IO.nwrite ch "    RuntimeVisibleParameterAnnotations = ";
      List.iter
        (fun al -> 
           IO.nwrite ch "\n      [";
           dump_list (fun () -> IO.nwrite ch ", ") (dump_annotation ch) al;
           IO.write ch ']')
        all;
      IO.write ch '\n'
  | AttributeUnknown (s,_) ->
      IO.nwrite ch ("    ?"^s^"\n")

let dump_field ch consts f =
  IO.printf ch "  %s%s %s\n" (access_flags f.f_flags) (value_signature f.f_descriptor) f.f_name;
  List.iter (dump_attrib ch consts) f.f_attributes

let dump_method ch consts m =
  IO.printf ch "  %s%s\n" (access_flags m.m_flags) (method_signature m.m_name m.m_descriptor);
  List.iter (dump_attrib ch consts) m.m_attributes;
  IO.write ch '\n'

let dump_super ch = function
  | None -> ()
  | Some c -> IO.printf ch "  extends %s\n" (class_name c)

let dump ch cl =
  IO.nwrite ch (access_flags cl.j_flags);
  if List.for_all ((<>) `AccInterface) cl.j_flags
  then IO.nwrite ch "class ";
  IO.nwrite ch (cn_name cl.j_name);
  IO.write ch '\n';
  dump_super ch cl.j_super;
  if cl.j_interfaces <> []
  then
    IO.printf ch "  implements %s\n"
      (String.concat " " (List.map class_name cl.j_interfaces));
  IO.nwrite ch ("    version = " ^(string_of_int cl.j_version.major)
                ^"." ^(string_of_int cl.j_version.minor) ^"\n");
  List.iter (dump_attrib ch cl.j_consts) cl.j_attributes;
  IO.printf ch "{\n\n";
  IO.printf ch "/* **** CONSTANTS ****\n";
  (* Put this in the dump method for high level class files *)
  Array.iteri
    (fun i c ->
       IO.nwrite ch ("    " ^string_of_int i^"  ");
       dump_constant ch c;
       IO.write ch '\n'
    ) cl.j_consts;
  IO.printf ch "// ****************** */\n\n";
  List.iter (dump_field ch cl.j_consts) cl.j_fields;
  IO.printf ch "\n";
  List.iter (dump_method ch cl.j_consts) cl.j_methods;
  IO.printf ch "}\n";
