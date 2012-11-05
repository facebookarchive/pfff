(*
 * This file is part of Javalib
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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

type jconst = [
  | `ANull (* AConstNull  *)
  | `Int of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `Byte of int (* BIPush *)
  | `Short of int
  | `String of jstr
  | `Class of object_type
]

type jopcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int

  (* Stack permutation *)
  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  (* Constant loading / it corresponds to instructions *const* and ldc* *)
  | OpConst of jconst

  (* Arithmetic *)
  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  (* Logic *)
  | OpIShl (* Use an I/L argument *)
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

  (* Conversion *)
  | OpI2L (* Use `I of [`L | `F  | `D] *)
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
  | OpI2B (* Those three are different *)
  | OpI2C
  | OpI2S

  | OpCmp of [`L | `FL | `FG | `DL | `DG]

  (* Conditional jump *)
  | OpIf of [`Eq | `Ne | `Lt | `Ge | `Gt | `Le | `Null | `NonNull] * int
  | OpIfCmp of [`IEq | `INe | `ILt | `IGe | `IGt | `ILe | `AEq | `ANe] * int

  (* Unconditional jump *)
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  (* Heap and static fields *)
  | OpNew of class_name
  | OpNewArray of value_type
  | OpAMultiNewArray of object_type * int (* ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * field_signature
  | OpPutStatic of class_name * field_signature
  | OpGetField of class_name * field_signature
  | OpPutField of class_name * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke of [
    | `Virtual of object_type
    | `Special of class_name
    | `Static of class_name
    | `Interface of class_name
    ]
    * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid

type jopcodes = jopcode array

(* Exception handler. *)
type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : class_name option
}

type jcode = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : jopcodes;
  c_exc_tbl : exception_handler list;
  c_line_number_table : (int * int) list option;
  c_local_variable_table : (int * int * string * value_type * int) list option;
  c_local_variable_type_table : (int * int * string * JSignature.fieldTypeSignature * int) list option;
  c_stack_map_midp : stackmap list option;
  c_stack_map_java6 : stackmap list option;
  c_attributes : (string * string) list;
}

let get_local_variable_info i pp code =
  match code.c_local_variable_table with
    | None -> None
    | Some lvt ->
        let offset =
          (* when an [store v] is done, [v] will have its type at the
             next program point.  Therefore, the LocalVariableTable
             only refers to [v] from the next program point.  To have
             the name and type of [v] we therefore need to look at the
             next program point. *)
          let code = code.c_code in
	    match code.(pp) with
	      | OpStore _ ->
                  let i = ref (pp + 1) in
                    while !i < Array.length code && code.(!i) = OpInvalid do
                      incr i
                    done;
                    !i - pp
	      | _ -> 0
        in
	  try
	    let (_,_,s,sign,_) =
	      List.find
		(fun (start,len,_,_,index) ->
		   pp + offset >= start
                   && pp + offset < start + len
                   && index = i
                ) lvt
            in
              Some (s,sign)
          with _ -> None

let get_source_line_number' pp lnt =
  let rec find_line prev = function
    | (start_pc,line_number)::r ->
	if (start_pc > pp) then Some prev
	else find_line line_number r
    | [] -> Some prev
  in
    try find_line (snd (List.hd lnt)) lnt
    with _ -> None

let get_source_line_number pp code =
  match code.c_line_number_table with
    | None -> None
    | Some lnt ->
        get_source_line_number' pp lnt
