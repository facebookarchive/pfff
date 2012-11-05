(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007, 2008 Tiphaine Turpin (Université de Rennes 1)
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

let replace_dot s =
  let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '.' then s.[i] <- '/'
    done;
    s

let class_name ?(jvm=false) cn =
  let cname = cn_name cn in
    if jvm then
      replace_dot cname
    else cname

let sprintf = Printf.sprintf

let basic_type ?(jvm=false) bt =
  match bt with
    | `Bool ->
	if jvm then "Z" else "bool"
    | `Byte ->
	if jvm then "B" else "byte"
    | `Char ->
	if jvm then "C" else "char"
    | `Double ->
	if jvm then "D" else "double"
    | `Float ->
	if jvm then "F" else "float"
    | `Int ->
	if jvm then "I" else "int"
    | `Long ->
	if jvm then "J" else "long"
    | `Short ->
	if jvm then "S" else "short"


let rec object_value_signature ?(jvm=false) ot =
  match ot with
    | TClass cn -> 
	let cn = class_name ~jvm:jvm cn in
	  if jvm then "L" ^cn^";"
	  else cn
    | TArray vt ->
	if jvm then
	  "[" ^ (value_signature ~jvm:true vt)
	else (value_signature vt) ^ "[]"

and value_signature ?(jvm=false) vt =
  match vt with
    | TBasic bt -> basic_type ~jvm:jvm bt
    | TObject ot -> object_value_signature ~jvm:jvm ot

let type2shortstring = value_signature ~jvm:true

let rettype2shortstring ?(jvm=true) = function
    None ->
	if jvm then "V"
	else "void"
    | Some v -> value_signature ~jvm:jvm v 

let arraytype2shortstring = function
  | `Long -> "J"
  | `Float -> "F"
  | `Double -> "D"
  | `Int -> "I"
  | `Short -> "S"
  | `Char -> "C"
  | `ByteBool -> "B"
  | `Object -> "A"

let method_signature name (sl,sr) =
		(match sr with
		| None -> "void"
		| Some s -> value_signature s) ^ " " ^name^ "(" ^ String.concat "," (List.map value_signature sl) ^ ")"

let signature name = function
  | SValue v -> value_signature v ^ " " ^name
  | SMethod m -> method_signature name m

let jvm_basic_type = function
	| `Int
	| `Int2Bool -> 'i'
	| `Long -> 'l'
	| `Float -> 'f'
	| `Double -> 'd'

let java_basic_type = function
  | `Int -> 'i'
  | `Long -> 'l'
  | `Float -> 'f'
  | `Double -> 'd'
  | `Short -> 's'
  | `Char -> 'c'
  | `Byte
  | `Bool -> 'b'

let jvm_array_type = function
  | `Int -> 'i'
  | `Long -> 'l'
  | `Float -> 'f'
  | `Double -> 'd'
  | `Short -> 's'
  | `Char -> 'c'
  | `ByteBool -> 'b'
  | `Object -> 'a'

let dump_constant_value ch = function
  | ConstString s -> IO.printf ch "string '%s'" (jstr_pp s)
  | ConstInt i -> IO.printf ch "int %ld" i
  | ConstFloat f -> IO.printf ch "float %f" f
  | ConstLong i -> IO.printf ch "long %Ld" i
  | ConstDouble f -> IO.printf ch "double %f" f
  | ConstClass cl -> IO.printf ch "class %s" (object_value_signature cl)

let dump_constant ch = function
  | ConstValue v -> dump_constant_value ch v
  | ConstField (cn,fs) ->
      let fn = fs_name fs
      and ft = fs_type fs
      in
        IO.printf ch "field : %s %s::%s" (value_signature ft) (class_name cn) fn
  | ConstMethod (cl,ms) ->
      let mn = ms_name ms
      and md = ms_args ms, ms_rtype ms
      in
        IO.printf ch "method : %s"
          (method_signature (object_value_signature cl ^ "::" ^ mn) md)
  | ConstInterfaceMethod (cn,ms) ->
      let mn = ms_name ms
      and md = ms_args ms, ms_rtype ms
      in
        IO.printf ch "interface-method : %s"
          (method_signature (class_name cn ^ "::" ^ mn) md)
  | ConstNameAndType (s,sign) -> IO.printf ch "name-and-type : %s" (signature s sign)
  | ConstStringUTF8 s -> IO.printf ch "utf8 %s" s
  | ConstUnusable -> IO.printf ch "unusable"

let dump_constantpool ch =
  Array.iteri
    (fun i c ->
      IO.printf ch "    %d  " i;
      dump_constant ch c;
      IO.write ch '\n')


let dump_verification_type = function
  | VTop -> "Top"
  | VInteger -> "Integer"
  | VFloat -> "Float"
  | VDouble -> "Double"
  | VLong -> "Long"
  | VNull -> "Null"
  | VUninitializedThis -> "UninitializedThis"
  | VObject c -> sprintf "Object %s" (object_value_signature c)
  | VUninitialized off -> sprintf "Uninitialized %d" off

let dump_stackmap ch (offset,locals,stack) =
  IO.printf ch "\n      offset=%d,\n      locals=[" offset;
  List.iter (fun t -> IO.printf ch "\n        %s" (dump_verification_type t)) locals;
  IO.printf ch "],\n      stack=[";
  List.iter (fun t -> IO.printf ch "\n        %s" (dump_verification_type t)) stack

open JCode

let dump_exc ch _cl exc =
  IO.printf ch "\n      [%d-%d] -> %d (" exc.e_start exc.e_end exc.e_handler;
  (match exc.e_catch_type with
     | None -> IO.printf ch "<finally>"
     | Some cl -> IO.printf ch "class %s" (class_name cl));
  IO.printf ch ")"


