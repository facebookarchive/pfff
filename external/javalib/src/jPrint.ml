(*
 * This file is part of Javalib
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
 * Copyright (c)2007, 2008, 2009 Laurent Hubert (CNRS)
 * Copyright (c)2009 Nicolas Barre (INRIA)
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
open JClass

let class_name  = JDumpBasics.class_name

let cn_package cn =
  String.concat "." (cn_package cn)

let cn_simple_name = cn_simple_name

let java_basic_type  = JDumpBasics.basic_type

let object_type = JDumpBasics.object_value_signature

let value_type = JDumpBasics.value_signature

let field_descriptor = value_type

let value_type_list ?(jvm=false) ?names l =
  let prms =
    match names with
      | None -> List.map value_type l
      | Some names ->
	  (* names must have the same length than l *)
	  try
	    List.map2
	      (fun v name ->
		 (value_type ~jvm:jvm v) ^ " " ^ name) l names
	  with
	      _ -> invalid_arg "Bad length for names list."
  in
    "(" ^ (String.concat ", " prms) ^ ")"

let return_type ?(jvm=false) = JDumpBasics.rettype2shortstring ~jvm:jvm

let method_descriptor ?(jvm=false) args ret =
  let ret = return_type ~jvm:jvm ret in
    if jvm then
      "(" ^ (String.concat "" (List.map (value_type ~jvm:true) args))
      ^ ")" ^ ret
    else
      ret ^ " " ^ (value_type_list args)

let method_signature ?(jvm=false) ?callee ?param_names ms =
  let mname = ms_name ms in
  let margs = ms_args ms in
  let mrtype = ms_rtype ms in
  let cname =
    match callee with
      | None -> ""
      | Some ot -> (object_type ot) ^ "." in
    if jvm then
      cname ^ mname ^ ":" ^ (method_descriptor ~jvm:true margs mrtype)
    else
      let ret = return_type mrtype
      and args =
        match param_names with
          | Some names -> value_type_list ~names margs
          | None -> value_type_list margs
      in
	ret ^ " " ^ cname ^ mname ^ args

let class_method_signature ?jvm ?param_names cms =
  let (cn,ms) = cms_split cms in
  let callee = TClass cn in
    match param_names,jvm with
      | None, None
          -> method_signature ~callee ms
      | Some param_names, None
          -> method_signature ~param_names ~callee ms
      | None, Some jvm
          -> method_signature ~jvm ~callee ms
      | Some param_names, Some jvm
          -> method_signature ~jvm ~param_names ~callee ms

let field_signature ?(jvm=false) ?declared_in fs =
  let fname = fs_name fs in
  let fd = fs_type fs in
  let cn = match declared_in with
    | None -> ""
    | Some cn -> class_name cn ^"."
  in
    if jvm then
      cn ^ fname ^ ":" ^ (field_descriptor ~jvm:true fd)
    else
      (field_descriptor fd) ^ " " ^ cn ^ fname

let class_field_signature ?jvm cfs =
  let (declared_in,fs) = cfs_split cfs in
    match jvm with
      | None -> field_signature ~declared_in fs
      | Some jvm -> field_signature ~jvm ~declared_in fs

let signature ?(jvm=false) name d =
  match d with
    | SValue fd ->
	let fs = make_fs name fd in
	  field_signature ~jvm:jvm fs
    | SMethod md ->
	let ms = make_ms name (fst md) (snd md) in
	  method_signature ~jvm:jvm ms

let constant_value = function
  | ConstString s -> "string '" ^ (jstr_pp s) ^ "'"
  | ConstInt i -> "int " ^ (Int32.to_string i)
  | ConstFloat f -> "float " ^ (string_of_float f)
  | ConstLong i -> "long " ^ (Int64.to_string i)
  | ConstDouble f -> "double " ^ (string_of_float f)
  | ConstClass cl -> "class " ^ (object_type cl)

let constant = function
  | ConstValue v -> constant_value v
  | ConstField (cl,fs) ->
      "field : " ^ (field_signature ~jvm:false ~declared_in:cl fs)
  | ConstMethod (ot,ms) ->
      "method : " ^ (method_signature ~callee:ot ms)
  | ConstInterfaceMethod (cn,ms) ->
      "interface-method : " ^ (method_signature ~callee:(TClass cn) ms)
  | ConstNameAndType (s,d) -> "name-and-type : " ^ (signature s d)
  | ConstStringUTF8 s -> "utf8 " ^ s
  | ConstUnusable -> "unusable"

let constant_pool p =
  let s = ref "" in
    Array.iteri
      (fun i c ->
	 s :=
	   !s ^ "    " ^ (string_of_int i) ^ "  " ^ (constant c) ^ "\n") p;
    !s

let stack_map (offset,locals,stack) =
  let verif_info = function
    | VTop -> "Top"
    | VInteger -> "Integer"
    | VFloat -> "Float"
    | VDouble -> "Double"
    | VLong -> "Long"
    | VNull -> "Null"
    | VUninitializedThis -> "UninitializedThis"
    | VObject c -> "Object " ^(object_type c)
    | VUninitialized off -> "Uninitialized " ^(string_of_int off)
  in
  let s = ref (Printf.sprintf "\n      offset=%d,\n      locals=[" offset) in
    List.iter
      (fun t ->
	 s := !s ^ Printf.sprintf  "\n        %s" (verif_info t)) locals;
    s := !s ^ "],\n      stack=[";
    List.iter
      (fun t ->
	 s := Printf.sprintf "\n        %s" (verif_info t)) stack;
    !s

open JCode
let exception_handler exc =
  let s = ref (Printf.sprintf "\n      [%d-%d] -> %d ("
		 exc.e_start exc.e_end exc.e_handler) in
    (match exc.e_catch_type with
       | None -> s := !s ^ "<finally>"
       | Some cl -> s := !s ^ Printf.sprintf "class %s" (class_name cl));
    !s ^ ")"

(* TODO: take a local variable table as argument to replace variable number with
   their name. *)
let jopcode_jvm =
  let sprintf = Printf.sprintf in
    function
      | OpNop -> "nop"
      | OpConst x ->
          (match x with
	     | `ANull -> "aconstnull"
	     | `Int i -> "iconst " ^ (Int32.to_string i)
	     | `Long i -> "lconst %Ld" ^ (Int64.to_string i)
	     | `Float f -> "fconst " ^ (string_of_float f)
	     | `Double f -> "dconst " ^ (string_of_float f)
	     | `Byte n -> "bipush " ^ (string_of_int n)
	     | `Short a -> "sipush " ^ (string_of_int a)
	     | `Class c -> "ldc class " ^ (object_type ~jvm:true c)
	     | `String s -> "ldc string '"^ (jstr_pp s) ^"'")

      | OpLoad (k,n) ->
          (match k with
	     | `Object -> "aload " ^(string_of_int n)
	     | `Int2Bool | `Long | `Float | `Double as k ->
                 sprintf "%cload %d" (JDumpBasics.jvm_basic_type k) n)

      | OpArrayLoad k ->
          (match k with
	     | `Object -> "aaload"
	     | `ByteBool -> "baload"
	     | `Char -> "caload"
	     | `Short -> "saload"
	     | `Int -> "iaload"
	     | `Long | `Float | `Double as k ->
                 sprintf "%caload" (JDumpBasics.jvm_basic_type k))

      | OpStore (k,n) ->
          (match k with
	     | `Object -> "astore " ^(string_of_int n)
	     | `Int2Bool | `Long | `Float | `Double as k ->
                 sprintf "%cstore %d" (JDumpBasics.jvm_basic_type k) n)

      | OpArrayStore k ->
          (match k with
	     | `Object -> "aastore"
	     | `ByteBool -> "bastore"
	     | `Char -> "castore"
	     | `Short -> "sastore"
	     | `Int -> "iastore"
	     | `Long | `Float | `Double as k ->
                 sprintf "%castore" (JDumpBasics.jvm_basic_type k))

      | OpPop -> "pop"
      | OpPop2 -> "pop2"
      | OpDup -> "dup"
      | OpDupX1 -> "dupX1"
      | OpDupX2 -> "dupX2"
      | OpDup2 -> "dup2"
      | OpDup2X1 -> "dup2X1"
      | OpDup2X2 -> "dup2X2"
      | OpSwap -> "swap"

      | OpAdd k -> sprintf "%cadd" (JDumpBasics.jvm_basic_type k)
      | OpSub k -> sprintf "%csub" (JDumpBasics.jvm_basic_type k)
      | OpMult k -> sprintf "%cmult" (JDumpBasics.jvm_basic_type k)
      | OpDiv k -> sprintf "%cdiv" (JDumpBasics.jvm_basic_type k)
      | OpRem k -> sprintf "%crem" (JDumpBasics.jvm_basic_type k)
      | OpNeg k -> sprintf "%cneg" (JDumpBasics.jvm_basic_type k)

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

      | OpIInc (a,b) -> "iinc "^(string_of_int a)^" "^ (string_of_int b)

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

      | OpCmp x ->
          (match x with
	     | `L -> "lcmp"
	     | `FL -> "fcmpl"
	     | `FG -> "fcmpg"
	     | `DL -> "dcmpl"
	     | `DG -> "dcmpg")
      | OpIf (x, n) ->
          (match x with
	       `Eq -> "ifeq " ^ (string_of_int n)
	     | `Ne -> "ifne " ^ (string_of_int n)
	     | `Lt -> "iflt " ^ (string_of_int n)
	     | `Ge -> "ifge " ^ (string_of_int n)
	     | `Gt -> "ifgt " ^ (string_of_int n)
	     | `Le -> "ifle " ^ (string_of_int n)
	     | `Null -> "ifnull " ^ (string_of_int n)
	     | `NonNull -> "ifnonnull " ^ (string_of_int n))
      | OpIfCmp (x, n) ->
          (match x with
	       `IEq -> "ifcmpeq " ^ (string_of_int n)
	     | `INe -> "ifcmpne " ^ (string_of_int n)
	     | `ILt -> "ifcmplt " ^ (string_of_int n)
	     | `IGe -> "ifcmpge " ^ (string_of_int n)
	     | `IGt -> "ifcmpgt " ^ (string_of_int n)
	     | `ILe -> "ifcmpme " ^ (string_of_int n)
	     | `AEq -> "ifacmpeq " ^ (string_of_int n)
	     | `ANe -> "ifacmpne " ^ (string_of_int n))
      | OpGoto n -> "goto " ^ (string_of_int n)
      | OpJsr n -> "jsr " ^ (string_of_int n)
      | OpRet n -> "ret " ^ (string_of_int n)

      | OpTableSwitch (def,min,max,tbl) ->
          (* "tableswitch ([_:_] -> [_,_,_,...],default:_)" *)
          let inst = 
            "tableswitch (["^ Int32.to_string min
            ^":"^ Int32.to_string max ^"] -> ["
          and table = String.concat "," (Array.to_list (Array.map string_of_int tbl))
          in inst^table^"],default:"^ string_of_int def^")"

      | OpLookupSwitch (default,jumps) ->
          let inst =
	    List.fold_left
	      (fun s (int,offset) ->
                 s ^ Int32.to_string int ^"->" ^ string_of_int offset^ " | ")
	      "lookupswitch "
	      jumps
          in inst ^ "_ ->" ^string_of_int default

      | OpReturn k ->
          (match k with
	     | `Object -> "areturn"
	     | `Void -> "return"
	     | `Int2Bool | `Long | `Float | `Double as k ->
                 sprintf "%creturn" (JDumpBasics.jvm_basic_type k))

      | OpGetStatic (cs, fs) ->
	  "getstatic "^ (field_signature ~jvm:true ~declared_in:cs fs)
      | OpPutStatic (cs, fs) ->
	  "putstatic " ^ (field_signature ~jvm:true ~declared_in:cs fs)
      | OpPutField (cs, fs) ->
	  "putfield " ^ (field_signature ~jvm:true ~declared_in:cs fs)
      | OpGetField (cs, fs) ->
	  "getfield " ^ (field_signature ~jvm:true ~declared_in:cs fs)
      | OpInvoke (x, ms) ->
	  (match x with
	     | `Virtual t ->
	         "invokevirtual " ^
                   (method_signature ~jvm:true ~callee:t ms)
	     | `Special cs ->
	         "invokespecial " ^
                   (method_signature ~jvm:true ~callee:(TClass cs) ms)
	     | `Static cs ->
	         "invokestatic " ^
                   (method_signature ~jvm:true ~callee:(TClass cs) ms)
	     | `Interface cs ->
	         "invokeinterface " ^
                   (method_signature ~jvm:true ~callee:(TClass cs) ms)
	  )
      | OpNew cs -> "new " ^ (class_name cs)
      | OpNewArray t ->
          (match t with
	     | TBasic t -> "newarray " ^ (java_basic_type ~jvm:true t)
	     | TObject c ->
	         "anewarray " ^ (object_type ~jvm:true c)
          )
      | OpArrayLength -> "arraylength"
      | OpThrow -> "athrow"
      | OpCheckCast t -> "checkcast " ^ (object_type ~jvm:true t)
      | OpInstanceOf t -> "instanceof " ^ (object_type ~jvm:true t)
      | OpMonitorEnter -> "monitorenter"
      | OpMonitorExit -> "monitorexit"
      | OpAMultiNewArray (t,b) ->
          "amultinewarray " ^(object_type ~jvm:true t) ^ " " ^ (string_of_int b)
      | OpBreakpoint -> "breakpoint"

      | OpInvalid -> "invalid"

let jopcode ?(jvm=false) op =
  if jvm then jopcode_jvm op
  else
    match op with
      | OpNew cn -> "new " ^ (cn_name cn)
      | OpNewArray v ->
	  (match v with
	     | TBasic b -> Printf.sprintf "newarray %s" (java_basic_type b)
	     | TObject o ->
		 Printf.sprintf "anewarray %s" (object_type o)
	  )
      | OpAMultiNewArray (o,i) ->
	  Printf.sprintf "amultinewarray %s %d" (object_type o) i
      | OpCheckCast t -> Printf.sprintf "checkcast %s" (object_type t)
      | OpInstanceOf t -> Printf.sprintf "instanceof %s" (object_type t)
      | OpGetStatic (cn,fs) ->
	  Printf.sprintf "getstatic %s.%s : %s" (cn_name cn) (fs_name fs)
	    (value_type (fs_type fs))
      | OpPutStatic (cn,fs) ->
	  Printf.sprintf "putstatic %s.%s : %s" (cn_name cn) (fs_name fs)
	    (value_type (fs_type fs))
      | OpGetField (cn,fs) ->
	  Printf.sprintf "getfield %s.%s : %s" (cn_name cn) (fs_name fs)
	    (value_type (fs_type fs))
      | OpPutField (cn,fs) ->
	  Printf.sprintf "putfield %s.%s : %s" (cn_name cn) (fs_name fs)
	    (value_type (fs_type fs))
      | OpInvoke ((`Virtual o),ms) ->
	  Printf.sprintf "invokevirtual %s.%s%s : %s" (object_type o)
	    (ms_name ms) (value_type_list (ms_args ms))
	    (return_type (ms_rtype ms))
      | OpInvoke ((`Interface cn),ms) ->
	  Printf.sprintf "invokeinterface %s.%s%s : %s" (cn_name cn)
	    (ms_name ms) (value_type_list (ms_args ms))
	    (return_type (ms_rtype ms))
      | OpInvoke ((`Static cn),ms) ->
	  Printf.sprintf "invokestatic %s.%s%s : %s" (cn_name cn)
	    (ms_name ms) (value_type_list (ms_args ms))
	    (return_type (ms_rtype ms))
      | OpInvoke ((`Special cn),ms) ->
	  Printf.sprintf "invokespecial %s.%s%s : %s" (cn_name cn)
	    (ms_name ms) (value_type_list (ms_args ms))
	    (return_type (ms_rtype ms))
      | _ -> jopcode_jvm op

let jcode ?(jvm=false) code =
  let cl = Array.to_list (Array.mapi (fun i op -> (i,op)) code.c_code) in
    ExtList.List.filter_map
      (fun (i,op) ->
	 match op with
	   | OpInvalid -> None
	   | _ -> Some(Printf.sprintf "%d: %s" i (jopcode ~jvm:jvm op))
      ) cl

let access_to_string = function
  | `Default -> None
  | `Protected -> Some "protected"
  | `Public -> Some "public"
  | `Private -> Some "private"

let method_access m =
  access_to_string (get_method_visibility m)

let field_access f =
  access_to_string (get_field_visibility f)

let interface_or_class_access ioc =
  access_to_string (get_access ioc)

let method_kind m =
  match m with
    | AbstractMethod _ -> Some "abstract"
    | ConcreteMethod cm ->
	(match cm.cm_implementation with
	   | Java _ -> None
	   | Native -> Some "native"
	)

let field_kind = function
  | InterfaceField _ -> None
  | ClassField cf ->
      match cf.cf_kind with
	| Final -> Some "final"
	| Volatile -> Some "volatile"
	| NotFinal -> None

let method_static m =
  if (is_static_method m) then Some "static"
  else None

let field_static f =
  match f with
    | ClassField cf ->
	if cf.cf_static then Some "static"
	else None
    | InterfaceField _ -> Some "static"

let interface_or_class_abstract ioc =
  match ioc with
    | JInterface _ -> None
    | JClass c ->
	if c.c_abstract then Some "abstract"
	else None

let method_final m =
  if (is_final_method m) then Some "final"
  else None

let interface_or_class_final ioc =
  match ioc with
    | JInterface _ -> None
    | JClass c ->
	if c.c_final then Some "final"
	else None

let method_synchronized m =
  if (is_synchronized_method m) then Some "synchronized"
  else None

let acmethod ?(jvm=false) (f:'a -> string list) (m:'a jmethod) =
  let ms = get_method_signature m in
  let header =
    String.concat " " (ExtList.List.filter_map
			 (fun x -> x)
			 [method_access m; method_static m;
			  method_final m; method_synchronized m;
			  method_kind m]) in
  let header = if header = "" then header else header ^ " " in
    match m with
      | AbstractMethod _ ->
	  if jvm then
	    (method_signature ~jvm:true ms, [])
	  else
	    (Printf.sprintf "%s%s" header (method_signature ms), [])
      | ConcreteMethod cm ->
	  (match cm.cm_implementation with
	     | Native ->
		 if jvm then
		   (method_signature ~jvm:true ms, [])
		 else
		   (Printf.sprintf "%s%s" header (method_signature ms), [])
	     | Java impl ->
                 let impl = Lazy.force impl in
		   if jvm then
		     (method_signature ~jvm:true ms, f impl)
		   else
		     (Printf.sprintf "%s%s" header (method_signature ms),
		      f impl)
	  )

let any_field ?(jvm=false) (f : any_field) : string =
  let fs = get_field_signature f in
  let header =
    String.concat " " (ExtList.List.filter_map
			 (fun x -> x)
			 [field_access f; field_static f;
			  field_kind f]) in
  let header = if header = "" then header else header ^ " " in
  let init_value = 
    let value =
      match f with
	  InterfaceField ifd -> ifd.if_value
	| ClassField cf -> cf.cf_value
    in
      match value with
	  None -> ""
	| Some cfv -> " = "^constant_value cfv
  in
    if jvm then field_signature ~jvm:true fs
    else
      Printf.sprintf "%s%s%s" header (field_signature fs) init_value

(* TODO: print other things than code ?*)
let print_method_fmt jvm m (print_code: 'a -> Format.formatter -> unit) fmt =
  let indent_val = 3 in
  let ms = get_method_signature m in
  let header =
    String.concat " " (ExtList.List.filter_map
			 (fun x -> x)
			 [method_access m; method_static m;
			  method_final m; method_synchronized m;
			  method_kind m]) in
  let header = if header = "" then header else header ^ " " in
    (match m with
       | AbstractMethod _ ->
	   let header = 
	     if jvm then
	       method_signature ~jvm:true ms
	     else
	       Printf.sprintf "%s%s" header (method_signature ms)
	   in
	     Format.pp_print_string fmt (header^";");
	     Format.pp_force_newline fmt ();
	     ()
       | ConcreteMethod cm ->
	   (match cm.cm_implementation with
	      | Native ->
		  let header =
		    if jvm then
		      method_signature ~jvm:true ms
		    else
		      Printf.sprintf "%s%s" header (method_signature ms)
		  in
		    Format.pp_print_string fmt (header^";");
		    Format.pp_force_newline fmt ();
		    ()
	      | Java impl ->
                  let impl = Lazy.force impl in
		  let header = 
		    if jvm then
		      method_signature ~jvm:true ms
		    else
		      Printf.sprintf "%s%s" header (method_signature ms)
		  in
		    Format.pp_open_vbox fmt indent_val;
		    Format.pp_print_string fmt (header^"{");
		    Format.pp_force_newline fmt ();
		    print_code impl fmt;
		    Format.pp_close_box fmt ();
		    Format.pp_force_newline fmt ();
		    Format.pp_print_string fmt "}";
		    Format.pp_force_newline fmt ();
		    ()));
    ()

let print_code (f: 'a -> string list) code fmt =
  let instructions = f code in
  let len = List.length instructions in
    ExtList.List.iteri
      (fun i s ->
	 Format.pp_print_string fmt s;
	 if (i < len - 1) then Format.pp_force_newline fmt ()) instructions;
    ()

let print_method ?(jvm=false) (m:'a jmethod) (f:'a -> string list)
    (out:out_channel) =
  let fmt = Format.formatter_of_out_channel out in
    print_method_fmt jvm m (print_code f) fmt;
    Format.pp_print_flush fmt ()

let print_method' ?(jvm=false) (m:'a jmethod) 
    (print_code: 'a -> Format.formatter -> unit) fmt =
  print_method_fmt jvm m print_code fmt;
  ()

let print_class_fmt ?(jvm=false) indent_val (ioc:'a interface_or_class) 
    print_code fmt =
  let name = cn_name (get_name ioc) in
  let impl_ext = 
    let extends = 
      match ioc with
	  JClass jc -> 
	    (match jc.c_super_class with
		 None -> ""
	       | Some cn -> "extends "^cn_name cn^" ")
	| _ -> ""
    in
    let get_output = function
	[] -> extends^""
      | l -> 
	  List.fold_left
	    (fun msg cn -> msg^" "^cn_name cn)
	    (extends^"implements")
	    l
    in
    let interf_l = 
      match ioc with
	  JClass jc -> jc.c_interfaces
	| JInterface ji -> ji.i_interfaces
    in
      get_output interf_l
  in
  let header = String.concat " "
    (ExtList.List.filter_map
       (fun x -> x)
       [interface_or_class_access ioc; interface_or_class_final ioc;
	interface_or_class_abstract ioc]) in
  let header = if header = "" then header else header ^ " " in
  let fields = get_fields ioc in
    match ioc with
      | JInterface i ->
	  Format.pp_print_string fmt
	    (Printf.sprintf "%sinterface %s %s{" header name impl_ext);
	  Format.pp_force_newline fmt ();
	  if not(FieldMap.is_empty fields) then
	    begin
	      Format.pp_open_vbox fmt indent_val;
	      Format.pp_force_newline fmt ();
	      FieldMap.iter
		(fun _ f ->
		   Format.pp_print_string fmt (any_field ~jvm:jvm f);
		   Format.pp_force_newline fmt ();
		) fields;
	      Format.pp_close_box fmt ();
	      Format.pp_force_newline fmt ()
	    end;
	  if not(MethodMap.is_empty i.i_methods)
	  then
	    begin
	      Format.pp_open_vbox fmt indent_val;
	      Format.pp_force_newline fmt ();
	      MethodMap.iter
		(fun _ m ->
		   print_method_fmt jvm (AbstractMethod m) print_code fmt;
		   Format.pp_force_newline fmt ();
		) i.i_methods;
	      Format.pp_close_box fmt ();
	      Format.pp_force_newline fmt ();
	    end;
	  Format.pp_print_string fmt "}";
	  Format.pp_force_newline fmt ();
	  ()
      | JClass c ->
	  Format.pp_print_string fmt
	    (Printf.sprintf "%sclass %s %s{" header name impl_ext);
	  Format.pp_force_newline fmt ();
	  if not(fields = FieldMap.empty) then
	    begin
	      Format.pp_open_vbox fmt indent_val;
	      Format.pp_force_newline fmt ();
	      FieldMap.iter
		(fun _ f ->
		   Format.pp_print_string fmt (any_field ~jvm:jvm f);
		   Format.pp_force_newline fmt ();
		) fields;
	      Format.pp_close_box fmt ();
	      Format.pp_force_newline fmt ()
	    end;
	  if not(MethodMap.is_empty c.c_methods)
	  then
	    begin
	      Format.pp_open_vbox fmt indent_val;
	      Format.pp_force_newline fmt ();     
	      MethodMap.iter
		(fun _ m ->
		   print_method_fmt jvm m print_code fmt;
		   Format.pp_force_newline fmt ();
		) c.c_methods;
	      Format.pp_close_box fmt ();
	      Format.pp_force_newline fmt ();
	    end;
	  Format.pp_print_string fmt "}";
	  Format.pp_force_newline fmt ();
	  ()

(* TODO: using a string list for instruction is not efficient (string
   concatenation is expensive) *)
let print_class ?(jvm=false) (ioc:'a interface_or_class) (f:'a -> string list)
    (out:out_channel) =
  let indent_val = 3 in
  let fmt = Format.formatter_of_out_channel out in
    print_class_fmt ~jvm indent_val ioc (print_code f) fmt;
    Format.pp_print_flush fmt ()

let print_class' ?(jvm=false) (ioc:'a interface_or_class) 
    (print_code:'a -> Format.formatter -> unit)
    (fmt:Format.formatter) =
  let indent_val = 3 in
    print_class_fmt ~jvm indent_val ioc print_code fmt

let print_jasmin c ch =
  let ioch = IO.output_channel ch in
  let clow = JHigh2Low.high2low c in
    JDumpJasmin.dump ioch clow
