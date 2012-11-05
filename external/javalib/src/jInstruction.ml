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

open JClass
open JBasics
open JBasicsLow
open JCode
open JClassLow
open IO
open IO.BigEndian

let count =
  List.fold_left
    (fun n vt ->
       n + match vt with
	 | JBasics.TBasic (`Double | `Long) -> 2
	 | _ -> 1)
    1

let opcode2instruction consts = function
  | OpNop -> JCode.OpNop
  | OpAConstNull -> JCode.OpConst `ANull
  | OpIConst v -> JCode.OpConst (`Int v)
  | OpLConst v -> JCode.OpConst (`Long v)
  | OpFConst v -> JCode.OpConst (`Float v)
  | OpDConst v -> JCode.OpConst (`Double v)
  | OpBIPush v -> JCode.OpConst (`Byte v)
  | OpSIPush v -> JCode.OpConst (`Short v)
  | OpLdc1 n
  | OpLdc1w n ->
      JCode.OpConst
	(match get_constant_value consts n with
	   | ConstInt c -> `Int c
	   | ConstFloat c -> `Float c
	   | ConstString c -> `String c
	   | ConstClass c -> `Class c
	   | ConstLong _ | ConstDouble _ -> raise (Class_structure_error ("Illegal constant for Ldc1: long/double")))
  | OpLdc2w n ->
      JCode.OpConst
	(match get_constant_value consts n with
	   | ConstInt _ | ConstFloat _ | ConstString _ | ConstClass _ ->
	       raise (Class_structure_error ("Illegal constant for Ldc2: int/float/string/class"))
	   | ConstLong c -> `Long c
	   | ConstDouble c -> `Double c)

  | OpLoad (k, l) ->
      JCode.OpLoad ((k : jvm_basic_type :> [> jvm_basic_type]), l)
  | OpALoad l -> JCode.OpLoad (`Object, l)

  | OpArrayLoad k ->
      JCode.OpArrayLoad (k : [`Int | other_num] :> [> `Int | other_num])
  | OpAALoad -> JCode.OpArrayLoad `Object
  | OpBALoad -> JCode.OpArrayLoad `ByteBool
  | OpCALoad -> JCode.OpArrayLoad `Char
  | OpSALoad -> JCode.OpArrayLoad `Short


  | OpStore (k, l) ->
      JCode.OpStore ((k : jvm_basic_type :> [> jvm_basic_type]), l)
  | OpAStore l -> JCode.OpStore (`Object, l)

  | OpArrayStore k ->
      JCode.OpArrayStore (k : [`Int | other_num] :> [> `Int | other_num])

  | OpAAStore -> JCode.OpArrayStore `Object
  | OpBAStore -> JCode.OpArrayStore `ByteBool
  | OpCAStore -> JCode.OpArrayStore `Char
  | OpSAStore -> JCode.OpArrayStore `Short

  | OpPop -> JCode.OpPop
  | OpPop2 -> JCode.OpPop2
  | OpDup -> JCode.OpDup
  | OpDupX1 -> JCode.OpDupX1
  | OpDupX2 -> JCode.OpDupX2
  | OpDup2 -> JCode.OpDup2
  | OpDup2X1 -> JCode.OpDup2X1
  | OpDup2X2 -> JCode.OpDup2X2
  | OpSwap -> JCode.OpSwap

  | OpAdd k -> JCode.OpAdd k
  | OpSub k -> JCode.OpSub k
  | OpMult k -> JCode.OpMult k
  | OpDiv k -> JCode.OpDiv k
  | OpRem k -> JCode.OpRem k
  | OpNeg k -> JCode.OpNeg k

  | OpIShl -> JCode.OpIShl
  | OpLShl -> JCode.OpLShl
  | OpIShr -> JCode.OpIShr
  | OpLShr -> JCode.OpLShr
  | OpIUShr -> JCode.OpIUShr
  | OpLUShr -> JCode.OpLUShr
  | OpIAnd -> JCode.OpIAnd
  | OpLAnd -> JCode.OpLAnd
  | OpIOr -> JCode.OpIOr
  | OpLOr -> JCode.OpLOr
  | OpIXor -> JCode.OpIXor
  | OpLXor -> JCode.OpLXor

  | OpIInc (index, incr) -> JCode.OpIInc (index, incr)

  | OpI2L -> JCode.OpI2L
  | OpI2F -> JCode.OpI2F
  | OpI2D -> JCode.OpI2D
  | OpL2I -> JCode.OpL2I
  | OpL2F -> JCode.OpL2F
  | OpL2D -> JCode.OpL2D
  | OpF2I -> JCode.OpF2I
  | OpF2L -> JCode.OpF2L
  | OpF2D -> JCode.OpF2D
  | OpD2I -> JCode.OpD2I
  | OpD2L -> JCode.OpD2L
  | OpD2F -> JCode.OpD2F
  | OpI2B -> JCode.OpI2B
  | OpI2C -> JCode.OpI2C
  | OpI2S -> JCode.OpI2S

  | OpLCmp -> JCode.OpCmp `L
  | OpFCmpL -> JCode.OpCmp `FL
  | OpFCmpG -> JCode.OpCmp `FG
  | OpDCmpL -> JCode.OpCmp `DL
  | OpDCmpG -> JCode.OpCmp `DG
  | OpIfEq pc -> JCode.OpIf (`Eq, pc)
  | OpIfNe pc -> JCode.OpIf (`Ne, pc)
  | OpIfLt pc -> JCode.OpIf (`Lt, pc)
  | OpIfGe pc -> JCode.OpIf (`Ge, pc)
  | OpIfGt pc -> JCode.OpIf (`Gt, pc)
  | OpIfLe pc -> JCode.OpIf (`Le, pc)
  | OpICmpEq pc -> JCode.OpIfCmp (`IEq, pc)
  | OpICmpNe pc -> JCode.OpIfCmp (`INe, pc)
  | OpICmpLt pc -> JCode.OpIfCmp (`ILt, pc)
  | OpICmpGe pc -> JCode.OpIfCmp (`IGe, pc)
  | OpICmpGt pc -> JCode.OpIfCmp (`IGt, pc)
  | OpICmpLe pc -> JCode.OpIfCmp (`ILe, pc)
  | OpACmpEq pc -> JCode.OpIfCmp (`AEq, pc)
  | OpACmpNe pc -> JCode.OpIfCmp (`ANe, pc)
  | OpGoto pc
  | OpGotoW pc -> JCode.OpGoto pc
  | OpJsr pc
  | OpJsrW pc -> JCode.OpJsr pc
  | OpRet l -> JCode.OpRet l

  | OpTableSwitch (def, low, high, tbl) -> JCode.OpTableSwitch  (def, low, high, tbl)
  | OpLookupSwitch (def, tbl) -> JCode.OpLookupSwitch (def, tbl)

  | OpReturn k -> JCode.OpReturn (k : jvm_basic_type :> [> jvm_basic_type])
  | OpAReturn -> JCode.OpReturn `Object
  | OpReturnVoid -> JCode.OpReturn `Void

  | OpGetStatic i ->
      let cs, fs = get_field consts i in
	JCode.OpGetStatic (cs, fs)
  | OpPutStatic i ->
      let cs, fs = get_field consts i in
	JCode.OpPutStatic (cs, fs)
  | OpGetField i ->
      let cs, fs = get_field consts i in
	JCode.OpGetField (cs, fs)
  | OpPutField i ->
      let cs, fs = get_field consts i in
	JCode.OpPutField (cs, fs)
  | OpInvokeVirtual i ->
      let t, ms = get_method consts i in
	JCode.OpInvoke (`Virtual t, ms)
  | OpInvokeNonVirtual i ->
      (match get_method consts i with
	 | TClass cs, ms ->
	     JCode.OpInvoke (`Special cs, ms)
	 | _ -> raise (Class_structure_error ("Illegal invokespecial: array class")))
  | OpInvokeStatic i ->
      (match get_method consts i with
	 | TClass cs, ms ->
	     JCode.OpInvoke (`Static cs, ms)
	 | _ -> raise (Class_structure_error ("Illegal invokestatic: array class")))
  | OpInvokeInterface (i, c) ->
      let cs, ms = get_interface_method consts i in
	if count (ms_args ms) <> c
	then raise (Class_structure_error "wrong count in invokeinterface");
	JCode.OpInvoke (`Interface cs, ms)

  | OpNew i -> JCode.OpNew (get_class consts i)
  | OpNewArray bt -> JCode.OpNewArray (TBasic bt)
  | OpANewArray i -> JCode.OpNewArray (TObject
					 (get_object_type consts i))
  | OpArrayLength -> JCode.OpArrayLength
  | OpThrow -> JCode.OpThrow
  | OpCheckCast i -> JCode.OpCheckCast (get_object_type consts i)
  | OpInstanceOf i -> JCode.OpInstanceOf (get_object_type consts i)
  | OpMonitorEnter -> JCode.OpMonitorEnter
  | OpMonitorExit -> JCode.OpMonitorExit
  | OpAMultiNewArray (ot, dims) -> JCode.OpAMultiNewArray
      ((get_object_type consts ot), dims)
  | OpIfNull pc -> JCode.OpIf (`Null, pc)
  | OpIfNonNull pc -> JCode.OpIf (`NonNull, pc)
  | OpBreakpoint -> JCode.OpBreakpoint

  | OpInvalid -> JCode.OpInvalid

let opcodes2code consts opcodes =
  Array.map (opcode2instruction consts) opcodes

let instruction2opcode consts length = function
  | JCode.OpNop -> OpNop
  | JCode.OpConst x ->
      let opldc_w c =
	let index = (value_to_int consts c)
	in
          if length = 2 && index <= 0xFF
          then OpLdc1 index
          else if length = 3
          then OpLdc1w index
          else
            raise
              (Class_structure_error
                 ("OpConst cannot be encoded in "^string_of_int length^" bytes."))
      in
	(match x with
	   | `ANull -> OpAConstNull
	   | `Int v ->
	       if length = 1 && -1l <= v && v <= 5l
	       then OpIConst v
	       else opldc_w (ConstInt v)
	   | `Long v ->
	       if length = 1 && (v=0L || v=1L)
	       then OpLConst v
	       else OpLdc2w (value_to_int consts (ConstLong v))
	   | `Float v ->
	       if length = 1 && (v=0. || v=1. || v=2.)
	       then OpFConst v
	       else opldc_w (ConstFloat v)
	   | `Double v ->
	       if length = 1 && (v=0. || v=1.)
               then OpDConst v
	       else OpLdc2w (value_to_int consts (ConstDouble v))
	   | `Byte v -> OpBIPush v
	   | `Short v -> OpSIPush v
	   | `String v -> opldc_w (ConstString v)
	   | `Class v -> opldc_w (ConstClass v)
	)

  | JCode.OpLoad (k, l) ->
      (match k with
	 | `Object -> OpALoad l
	 | #jvm_basic_type as k -> OpLoad (k, l))

  | JCode.OpArrayLoad k ->
      (match k with
	 | `Object -> OpAALoad
	 | `ByteBool -> OpBALoad
	 | `Char -> OpCALoad
	 | `Short -> OpSALoad
	 | `Int | #other_num as k -> OpArrayLoad k)

  | JCode.OpStore (k, l) ->
      (match k with
	 | `Object -> OpAStore l
	 | #jvm_basic_type as k -> OpStore (k, l))

  | JCode.OpArrayStore k ->
      (match k with
	 | `Object -> OpAAStore
	 | `ByteBool -> OpBAStore
	 | `Char -> OpCAStore
	 | `Short -> OpSAStore
	 | `Int | #other_num as k -> OpArrayStore k)

  | JCode.OpPop -> OpPop
  | JCode.OpPop2 -> OpPop2
  | JCode.OpDup -> OpDup
  | JCode.OpDupX1 -> OpDupX1
  | JCode.OpDupX2 -> OpDupX2
  | JCode.OpDup2 -> OpDup2
  | JCode.OpDup2X1 -> OpDup2X1
  | JCode.OpDup2X2 -> OpDup2X2
  | JCode.OpSwap -> OpSwap

  | JCode.OpAdd k -> OpAdd k
  | JCode.OpSub k -> OpSub k
  | JCode.OpMult k -> OpMult k
  | JCode.OpDiv k -> OpDiv k
  | JCode.OpRem k -> OpRem k
  | JCode.OpNeg k -> OpNeg k

  | JCode.OpIShl -> OpIShl
  | JCode.OpLShl -> OpLShl
  | JCode.OpIShr -> OpIShr
  | JCode.OpLShr -> OpLShr
  | JCode.OpIUShr -> OpIUShr
  | JCode.OpLUShr -> OpLUShr
  | JCode.OpIAnd -> OpIAnd
  | JCode.OpLAnd -> OpLAnd
  | JCode.OpIOr -> OpIOr
  | JCode.OpLOr -> OpLOr
  | JCode.OpIXor -> OpIXor
  | JCode.OpLXor -> OpLXor

  | JCode.OpIInc (index, incr) -> OpIInc (index, incr)

  | JCode.OpI2L -> OpI2L
  | JCode.OpI2F -> OpI2F
  | JCode.OpI2D -> OpI2D
  | JCode.OpL2I -> OpL2I
  | JCode.OpL2F -> OpL2F
  | JCode.OpL2D -> OpL2D
  | JCode.OpF2I -> OpF2I
  | JCode.OpF2L -> OpF2L
  | JCode.OpF2D -> OpF2D
  | JCode.OpD2I -> OpD2I
  | JCode.OpD2L -> OpD2L
  | JCode.OpD2F -> OpD2F
  | JCode.OpI2B -> OpI2B
  | JCode.OpI2C -> OpI2C
  | JCode.OpI2S -> OpI2S

  | JCode.OpCmp x ->
      (match x with
	 | `L -> OpLCmp
	 | `FL -> OpFCmpL
	 | `FG -> OpFCmpG
	 | `DL -> OpDCmpL
	 | `DG -> OpDCmpG)
  | JCode.OpIf (x, pc) ->
      (match x with
	   `Eq -> OpIfEq pc
	 | `Ne -> OpIfNe pc
	 | `Lt -> OpIfLt pc
	 | `Ge -> OpIfGe pc
	 | `Gt -> OpIfGt pc
	 | `Le -> OpIfLe pc
	 | `Null -> OpIfNull pc
	 | `NonNull -> OpIfNonNull pc)
  | JCode.OpIfCmp (x, pc) ->
      (match x with
	   `IEq -> OpICmpEq pc
	 | `INe -> OpICmpNe pc
	 | `ILt -> OpICmpLt pc
	 | `IGe -> OpICmpGe pc
	 | `IGt -> OpICmpGt pc
	 | `ILe -> OpICmpLe pc
	 | `AEq -> OpACmpEq pc
	 | `ANe -> OpACmpNe pc)
  | JCode.OpGoto pc ->
      if length = 3
      then OpGoto pc
      else if length = 5
      then OpGotoW pc
      else
        raise
          (Class_structure_error
             ("OpGoto "^string_of_int pc ^ " cannot be encoded in "
              ^ string_of_int length ^" bytes."))
  | JCode.OpJsr pc ->
      if length = 3
      then OpJsr pc
      else if length = 5
      then OpJsrW pc
      else
        raise
          (Class_structure_error
             ("OpJsr " ^ string_of_int pc ^ " cannot be encoded in "
              ^ string_of_int length ^" bytes."))
  | JCode.OpRet l -> OpRet l

  | JCode.OpTableSwitch (def, low, high, tbl) -> OpTableSwitch  (def, low, high, tbl)
  | JCode.OpLookupSwitch (def, tbl) -> OpLookupSwitch (def, tbl)

  | JCode.OpReturn k ->
      (match k with
	 | `Object -> OpAReturn
	 | `Void -> OpReturnVoid
	 | #jvm_basic_type as k -> OpReturn k)

  | JCode.OpGetStatic (cs, fs) ->
      OpGetStatic (field_to_int consts (cs,fs))
  | JCode.OpPutStatic (cs, fs) ->
      OpPutStatic (field_to_int consts (cs,fs))
  | JCode.OpGetField (cs, fs) ->
      OpGetField (field_to_int consts (cs, fs))
  | JCode.OpPutField (cs, fs) ->
      OpPutField (field_to_int consts (cs, fs))
  | JCode.OpInvoke (x, ms) ->
      (match x with
	 | `Virtual t ->
	     OpInvokeVirtual
	       (method_to_int consts (t, ms))
	 | `Special t ->
	     OpInvokeNonVirtual
	       (method_to_int consts (TClass t, ms))
	 | `Static t ->
	     OpInvokeStatic
	       (method_to_int consts (TClass t, ms))
	 | `Interface t ->
	     OpInvokeInterface
	       (constant_to_int consts
		  (ConstInterfaceMethod (t, ms)), count (ms_args ms))
      )

  | JCode.OpNew cs -> OpNew (class_to_int consts cs)
  | JCode.OpNewArray t ->
      (match t with
	 | TBasic bt -> OpNewArray bt
	 | TObject ot ->
	     OpANewArray (object_type_to_int consts ot)
      )
  | JCode.OpArrayLength -> OpArrayLength
  | JCode.OpThrow -> OpThrow
  | JCode.OpCheckCast ot -> OpCheckCast (object_type_to_int consts ot)
  | JCode.OpInstanceOf ot -> OpInstanceOf (object_type_to_int consts ot)
  | JCode.OpMonitorEnter -> OpMonitorEnter
  | JCode.OpMonitorExit -> OpMonitorExit
  | JCode.OpAMultiNewArray (i, dims) ->
      OpAMultiNewArray (object_type_to_int consts i, dims)
  | JCode.OpBreakpoint -> OpBreakpoint

  | JCode.OpInvalid -> OpInvalid

let check_space _consts offset length opcode =
  let ch = output_string () in
  let ch, count = pos_out ch in
  let offsetmod4 = offset mod 4 in
    for i = 1 to offsetmod4 do (* Pour les instructions alignées *)
      write_byte ch 0
    done;
    JParseCode.unparse_instruction ch count length opcode;
    let space_taken = count () - offsetmod4 in
    let opcodestring = close_out ch in
      if not (JBasics.get_permissive ()) && not  (String.length opcodestring - offsetmod4 = length)
      then failwith "check_space: count does not seems to provide the right result";
      length = space_taken


let code2opcodes consts code =
  let opcodes = Array.create (Array.length code) OpNop in
    Array.iteri
      (fun i instr ->
	 if instr <> JCode.OpInvalid
	 then (
           let length =
             let j = ref (i+1) in
               while !j < Array.length code && code.(!j) = JCode.OpInvalid do
                 opcodes.(!j) <- OpInvalid;
                 incr j
               done;
               !j-i
           in
	   let opcode = instruction2opcode consts length instr in
	     opcodes.(i) <- opcode;
	     if not (JBasics.get_permissive ()) && not (check_space consts i length opcode)
	     then
               raise (Class_structure_error "Low level translation of instruction is too long for the allocated space in high level code");
	 ))
      code;
    opcodes
