(*
 * This file is part of Javalib
 * Copyright (c)2004 Nicolas Cannasse
 * Copyright (c)2007 Tiphaine Turpin (Université de Rennes 1)
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


open JClassLow
open IO.BigEndian
open JBasics
open JSignature

(* Validate an utf8 string and return a stream of characters. *)
let read_utf8 s =
  UTF8.validate s;
  let index = ref 0 in
    Stream.from
      (function _ ->
	 if UTF8.out_of_range s ! index
	 then None
	 else
	   let c = UTF8.look s ! index in
	     index := UTF8.next s ! index;
	     Some c)

(* Java ident, with unicode letter and numbers, starting with a letter. *)
let parse_ident buff =
  let parse_char = parser
    | [< 'c when c <> UChar.of_char ';'  (* should be a letter or a number *)
	   && c <> UChar.of_char '/'
	   && c <> UChar.of_char ':'
	   && c <> UChar.of_char '.'
	   && c <> UChar.of_char '>'
	   && c <> UChar.of_char '<' >] -> c
  in
  let rec parse_more_ident buff = parser
      (* TODO : it seems to be relatively inefficient *)
    | [< c = parse_char;
	 name =
	  (UTF8.Buf.add_char buff c;
	   parse_more_ident buff) >] -> name
    | [< >] ->
	UTF8.Buf.contents buff
  in
    parser
      | [< c = parse_char; (* should be a letter *)
	   name =
	    (UTF8.Buf.add_char buff c;
	     parse_more_ident buff) >] -> name


(* Qualified name (internally encoded with '/'). *)
let rec parse_name = parser
  | [< ident = parse_ident (UTF8.Buf.create 0);
       name = parse_more_name >] -> ident :: name

and parse_more_name = parser
  | [< 'slash when slash = UChar.of_char '/';
       name = parse_name >] -> name
  | [< >] -> []

let get_name x = make_cn (String.concat "." (parse_name x))

(* Java type. *)
let parse_base_type = parser
  | [< 'b when b = UChar.of_char 'B' >] -> `Byte
  | [< 'c when c = UChar.of_char 'C' >] -> `Char
  | [< 'd when d = UChar.of_char 'D' >] -> `Double
  | [< 'f when f = UChar.of_char 'F' >] -> `Float
  | [< 'i when i = UChar.of_char 'I' >] -> `Int
  | [< 'j when j = UChar.of_char 'J' >] -> `Long
  | [< 's when s = UChar.of_char 'S' >] -> `Short
  | [< 'z when z = UChar.of_char 'Z' >] -> `Bool

let rec parse_object_type = parser
  | [< 'l when l = UChar.of_char 'L';
       name = get_name;
       'semicolon when semicolon = UChar.of_char ';' >] -> TClass name
  | [< a = parse_array >] -> a

and parse_array = parser
  | [< 'lbracket when lbracket = UChar.of_char '['; typ = parse_type >] ->
      TArray typ

and parse_type = parser
  | [< b = parse_base_type >] -> TBasic b
  | [< o = parse_object_type >] -> TObject o

let rec parse_types = parser
  | [< typ = parse_type ; types = parse_types >] -> typ :: types
  | [< >] -> []

let parse_type_option = parser
  | [< typ = parse_type >] -> Some typ
  | [< >] -> None

(* A class name, possibly an array class. *)
let parse_ot = parser
  | [< array = parse_array >] -> array
  | [< name = get_name >] -> TClass name

let parse_method_sig = parser
  | [< 'lpar when lpar = UChar.of_char '(';
       types = parse_types;
       'rpar when rpar = UChar.of_char ')';
       typ = parse_type_option >] ->
      (types, typ)

(* Java signature. *)
let rec parse_sig = parser
    (* We cannot delete that because of "NameAndType" constants. *)
  | [< typ = parse_type >] -> SValue typ
  | [< sign = parse_method_sig >] -> SMethod sign

let parse_objectType s =
  try
    parse_ot (read_utf8 s)
  with
      Stream.Failure -> raise (Class_structure_error ("Illegal object type: " ^ s))

let parse_field_descriptor s =
  try
    parse_type (read_utf8 s)
  with
      Stream.Failure -> raise (Class_structure_error ("Illegal type: " ^ s))

let parse_method_descriptor s =
  try
    parse_method_sig (read_utf8 s)
  with
      Stream.Failure -> raise (Class_structure_error ("Illegal method signature: " ^ s))

let parse_descriptor s =
  try
    parse_sig (read_utf8 s)
  with
      Stream.Failure -> raise (Class_structure_error ("Illegal signature: " ^ s))




(* Java 5 signature *)

(* this is the type of stream used in this code *)
type stream = UChar.t Stream.t

let parse_TypeVariableSignature : stream -> typeVariable = parser
  | [< 't when t = UChar.of_char 'T';
       name = parse_ident (UTF8.Buf.create 0);
       'semicolon when semicolon = UChar.of_char ';'>] -> TypeVariable name

let parse_QualifiedName : stream -> class_name = get_name

let rec parse_TypeArguments : stream -> typeArgument list =
  let parse_TypeArgument = parser
    | [< 'plus when plus = UChar.of_char '+'; typ = parse_FieldTypeSignature >]
      -> ArgumentExtends typ
    | [< 'minus when minus = UChar.of_char '-'; typ = parse_FieldTypeSignature >]
      -> ArgumentInherits typ
    | [< typ = parse_FieldTypeSignature >]
      -> ArgumentIs typ
    | [< 'star when star = UChar.of_char '*' >]
      -> ArgumentIsAny
  in
  let rec parse_more = parser
    | [< typ = parse_TypeArgument; other = parse_more >] -> typ::other
    | [< >] -> []
  in
    parser
      | [< 'lt when lt = UChar.of_char '<';
	   typ = parse_TypeArgument;
	   other = parse_more;
	   'gt when gt = UChar.of_char '>' >] -> typ::other
      | [< >] -> []


and parse_ArrayTypeSignature : stream -> typeSignature = parser
  | [< 'lb when lb = UChar.of_char '[';
       typ = parse_TypeSignature >] -> typ

and parse_TypeSignature : stream -> typeSignature = parser
  | [< ot = parse_FieldTypeSignature >] -> GObject ot
  | [< bt = parse_base_type >] -> GBasic bt


and parse_SimpleClassTypeSignature : stream -> simpleClassTypeSignature = parser
  | [< ident = parse_ident (UTF8.Buf.create 0); arguments = parse_TypeArguments >]
    -> {scts_name = ident; scts_type_arguments = arguments;}

and parse_ClassTypeSignatureSuffixes : stream -> simpleClassTypeSignature list =
  let parse_ClassTypeSignatureSuffix = parser
    | [< 'dot when dot = UChar.of_char '.'; ct = parse_SimpleClassTypeSignature >]
      -> ct
  in parser
    | [< ct = parse_ClassTypeSignatureSuffix ; others = parse_ClassTypeSignatureSuffixes >]
      -> ct::others
    | [< >] -> []

and parse_ClassTypeSignature : stream -> classTypeSignature = parser
  | [< 'l when l = UChar.of_char 'L';
       qualified_name = parse_QualifiedName;
       args = parse_TypeArguments;
       cts = parse_ClassTypeSignatureSuffixes;
       'semicolon when semicolon = UChar.of_char ';' >]
    ->
      let package = cn_package qualified_name in
      let ct = cn_simple_name qualified_name in
      let (current_class,enclosing_classes) =
	let rev = List.rev ({scts_name = ct;scts_type_arguments = args;}::cts) in
	let hd = List.hd rev in
	  (hd,List.rev (List.tl rev))
      in {
	  cts_package = package;
	  cts_enclosing_classes = enclosing_classes;
	  cts_simple_class_type_signature = current_class;}

and parse_FieldTypeSignature : stream -> fieldTypeSignature = parser
  | [< ct = parse_ClassTypeSignature >] -> GClass ct
  | [< at = parse_ArrayTypeSignature >] -> GArray at
  | [< tv = parse_TypeVariableSignature >] -> GVariable tv

let parse_ClassBound : stream -> fieldTypeSignature option = parser
  | [< 'colon when colon= UChar.of_char ':';
       e = (parser
	      | [< typ = parse_FieldTypeSignature >] -> Some typ
	      | [< >] -> None) >] -> e


let rec parse_InterfaceBounds : stream -> fieldTypeSignature list =
  let parse_InterfaceBound = parser
    | [< 'colon when colon= UChar.of_char ':'; typ = parse_FieldTypeSignature >] -> typ
  in parser
    | [< ib = parse_InterfaceBound; others = parse_InterfaceBounds >] -> ib::others
    | [< >] -> []


let parse_FormalTypeParameters : stream -> formalTypeParameter list =
  let parse_FormalTypeParameter : stream -> formalTypeParameter = parser
    | [< name = parse_ident (UTF8.Buf.create 0); cb = parse_ClassBound; ib = parse_InterfaceBounds >]
      -> {ftp_name = name; ftp_class_bound = cb; ftp_interface_bounds = ib;}
  in
  let rec parse_more = parser
    | [< typ = parse_FormalTypeParameter ; others = parse_more >] -> typ::others
    | [< >] -> []
  in parser
    | [< 'lt when lt = UChar.of_char '<';
	 typ = parse_FormalTypeParameter;
	 others = parse_more;
	 'gt when gt = UChar.of_char '>' >]
      -> typ::others
    | [< >] -> []

let parse_SuperclassSignature : stream -> classTypeSignature = parse_ClassTypeSignature
let rec parse_SuperinterfaceSignatures : stream -> classTypeSignature list = parser
  | [< sis = parse_ClassTypeSignature; others = parse_SuperinterfaceSignatures >] -> sis::others
  | [< >] -> []

let parse_ClassSignature : stream -> classSignature = parser
  | [< ftp = parse_FormalTypeParameters;
       scs = parse_SuperclassSignature;
       sis = parse_SuperinterfaceSignatures >] ->
      {cs_formal_type_parameters = ftp;
       cs_super_class = scs;
       cs_super_interfaces = sis;}


let parse_MethodTypeSignature : stream -> methodTypeSignature =
  let parse_ReturnType : stream -> typeSignature option = parser
    | [< 'v when v = UChar.of_char 'V' >] -> None
    | [< ts = parse_TypeSignature >] -> Some ts
  in
  let rec parse_ThrowsSignature : stream -> throwsSignature list = parser
    | [< 'circ when circ = UChar.of_char '^';
	 e = (parser
		| [< cl = parse_ClassTypeSignature >] -> ThrowsClass cl
		| [< var = parse_TypeVariableSignature >] -> ThrowsTypeVariable var);
	 others = parse_ThrowsSignature >] -> e::others
    | [< >] -> []
  and parse_TypeSignatures : stream -> typeSignature list = parser
    | [< ts = parse_TypeSignature ; others = parse_TypeSignatures >] -> ts::others
    | [< >] -> []
  in parser
    | [< fp = parse_FormalTypeParameters;
	 'lpar when lpar = UChar.of_char '(';
	 ts = parse_TypeSignatures;
	 'rpar when rpar = UChar.of_char ')';
	 rt = parse_ReturnType;
	 throws = parse_ThrowsSignature >]
      -> { mts_formal_type_parameters = fp;
	   mts_type_signature = ts;
	   mts_return_type = rt;
	   mts_throws = throws;}

let parse_ClassSignature (s:string) : classSignature =
  try
    parse_ClassSignature (read_utf8 s)
  with
    | Failure "tl"
    | Failure "hd"
    | Stream.Error _
    | Stream.Failure -> raise (Class_structure_error ("Ill-formed class Signature attribute: "^s))

let parse_MethodTypeSignature (s:string) : methodTypeSignature =
  try
    parse_MethodTypeSignature (read_utf8 s)
  with
    | Failure "tl"
    | Failure "hd"
    | Stream.Error _
    | Stream.Failure -> raise (Class_structure_error ("Ill-formed method Signature attribute: "^s))


let parse_FieldTypeSignature (s:string) : fieldTypeSignature =
  try
    parse_FieldTypeSignature (read_utf8 s)
  with
    | Failure "tl"
    | Failure "hd"
    | Stream.Error _
    | Stream.Failure -> raise (Class_structure_error ("Ill-formed field Signature attribute: "^s))


