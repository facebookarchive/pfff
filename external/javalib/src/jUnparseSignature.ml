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
open JClassLow
open JSignature

(* Descriptors and classname encoding *)
(************************************)

let encode_class_name cs =
  let cn = cn_name cs in
    ExtString.String.map
      (fun c -> if c = '.' then '/' else c) cn

let unparse_basic_type = function
  | `Byte -> "B"
  | `Char -> "C"
  | `Double -> "D"
  | `Float -> "F"
  | `Int -> "I"
  | `Long -> "J"
  | `Short -> "S"
  | `Bool -> "Z"

let rec unparse_object_type = function
  | TClass c ->
      "L" ^ encode_class_name c ^ ";"
  | TArray s ->
      "[" ^ unparse_value_type s

and unparse_value_type = function
  | TBasic b -> unparse_basic_type b
  | TObject o -> unparse_object_type o

let rec unparse_method_descriptor (sigs, s) =
      List.fold_left
	(fun desc s ->
	   desc ^ unparse_value_type s)
	"("
	sigs
      ^ ")"
      ^ (match s with
	   | Some s -> unparse_value_type s
	   | None -> "V")

let rec unparse_descriptor = function
  | SValue v -> unparse_value_type v
  | SMethod m -> unparse_method_descriptor m

(* Unparse a type that must be a constant of type class. Therefore, there is no 'L'
   and ';' around the class name (if this is a class). *)
let unparse_constClass = function
  | TClass c -> encode_class_name c
  | TArray _ as s -> unparse_object_type s


(** (generic) Signatures encoding as describe in the JVMS of Java 5
    (chapter 4.4.4). *)

(** *)

let unparse_TypeVariableSignature : typeVariable -> string =
  function TypeVariable s -> "T"^s^";"

let unparse_package : string list -> string = function
  | [] -> ""
  | pl -> String.concat "/" pl ^ "/"

let rec unparse_TypeArgument : typeArgument -> string = function
  | ArgumentExtends typ -> "+"^unparse_FieldTypeSignature typ
  | ArgumentInherits typ -> "-"^unparse_FieldTypeSignature typ
  | ArgumentIs typ -> unparse_FieldTypeSignature typ
  | ArgumentIsAny -> "*"

and unparse_TypeArguments : typeArgument list -> string = function
  | [] -> ""
  | l ->
      "<"
      ^ String.concat "" (List.map unparse_TypeArgument l)
      ^ ">"

and unparse_ArrayTypeSignature (ts:typeSignature) : string =
  "[" ^ unparse_TypeSignature ts

and unparse_TypeSignature : typeSignature -> string = function
  | GObject ot -> unparse_FieldTypeSignature ot
  | GBasic bt -> unparse_basic_type bt

and unparse_SimpleClassTypeSignature (scts: simpleClassTypeSignature) : string =
  scts.scts_name ^ unparse_TypeArguments scts.scts_type_arguments

and unparse_ClassTypeSignature (cts:classTypeSignature) : string =
  "L"
  ^ unparse_package cts.cts_package
  ^ String.concat "."
    (List.map
       unparse_SimpleClassTypeSignature
       (cts.cts_enclosing_classes @ [cts.cts_simple_class_type_signature]))
  ^ ";"

and unparse_FieldTypeSignature : fieldTypeSignature -> string = function
  | GClass ct -> unparse_ClassTypeSignature ct
  | GArray at -> unparse_ArrayTypeSignature at
  | GVariable vt -> unparse_TypeVariableSignature vt

and unparse_ClassBound : fieldTypeSignature option -> string = function
  | None -> ":"
  | Some cb -> ":" ^ unparse_FieldTypeSignature cb

and unparse_InterfaceBounds (ibs:fieldTypeSignature list) : string =
  String.concat "" (List.map (fun ib -> ":" ^ unparse_FieldTypeSignature ib) ibs)

and unparse_FormalTypeParameter (ftp:formalTypeParameter) : string =
  ftp.ftp_name
  ^ unparse_ClassBound ftp.ftp_class_bound
  ^ unparse_InterfaceBounds ftp.ftp_interface_bounds

and unparse_FormalTypeParameters :formalTypeParameter list -> string = function
  | [] -> ""
  | ftp ->
      "<"
      ^ String.concat "" (List.map unparse_FormalTypeParameter ftp)
      ^ ">"

let unparse_SuperclassSignature = unparse_ClassTypeSignature

let unparse_SuperinterfaceSignatures sis =
  String.concat "" (List.map unparse_ClassTypeSignature sis)

let unparse_ClassSignature (cs:classSignature) : string =
  unparse_FormalTypeParameters cs.cs_formal_type_parameters
  ^ unparse_SuperclassSignature cs.cs_super_class
  ^ unparse_SuperinterfaceSignatures cs.cs_super_interfaces


let unparse_MethodTypeSignature (mts:methodTypeSignature) : string =
  let unparse_ReturnType :typeSignature option -> string = function
    | None -> "V"
    | Some ts -> unparse_TypeSignature ts
  and unparse_ThrowsSignature (tsl:throwsSignature list) : string =
    String.concat ""
      (List.map
	 (function
	    | ThrowsClass cl -> "^" ^ unparse_ClassTypeSignature cl
	    | ThrowsTypeVariable var -> "^" ^ unparse_TypeVariableSignature var)
	 tsl)
  in
     unparse_FormalTypeParameters mts.mts_formal_type_parameters
    ^ "("
    ^ String.concat "" (List.map unparse_TypeSignature mts.mts_type_signature)
    ^ ")"
    ^ unparse_ReturnType mts.mts_return_type
    ^ unparse_ThrowsSignature mts.mts_throws

