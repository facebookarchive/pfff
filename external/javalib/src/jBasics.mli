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

(** Basic elements of class files. *)

(** {2 Definition of basic types and descriptors.} *)

(** Type representing a class name. e.g. [java.lang.Object] *)
type class_name

(** Type representing a method signature.
    A method signature contains the method name, the types of parameters
    and the method return type. Two methods in two different classes can
    have the same [method_signature]. *)
type method_signature

(** Type representing a field signature.
    A field signature contains the field name and the field type. *)
type field_signature

(** Type representing a signature for a field in a particular class.
    Each field of each class has a unique [class_field_signature]. *)
type class_field_signature

(** Type representing a signature for a method in a particular class.
    Each method of each class has a unique [class_method_signature]. *)
type class_method_signature

(** Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]

(** JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]

(** JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]

(** JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]

(** JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]

(** Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]

(** Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type

(** Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type

(** Version number of the class file. Extract of the specification: An
    implementation of Java 1.k sould support class file formats of
    versions in the range of 45.0 through 44+k.0 inclusive. E.g. Java
    1.6 implementations support class file formats of versions up to
    50.0. *)
type version = {major :int; minor:int;}

(** {2 Basic types manipulation.} *)

(** Creating and manipulating {!class_name} values. *)

(** [java.lang.Object] class name. *)
val java_lang_object : class_name

(** Builds a [class_name] from a string representing the Java fully qualified
    name of the class. e.g. "java.lang.Object".

    @raise Invalid_argument if the class name given as argument is not a valid
    class name (ie. it must match the regular expression
    ["^\\([a-zA-Z_$][a-zA-Z_$0-9]*\\.\\)*\\([a-zA-Z_0-9]+\\$\\)*[a-zA-Z_0-9]+$]").  *)
val make_cn : string -> class_name

(** Returns the hash value of the given [class_name]. *)
val cn_hash : class_name -> int

(** Retrieves the Java fully qualified name of a class.
    e.g. "java.lang.Object". *)
val cn_name : class_name -> string

(** Retrieves the Java simple name of a class, omitting the package name.
    e.g. "Object" instead of "java.lang.Object". *)
val cn_simple_name : class_name -> string

(** Retrieves the package name from a [class_name]. *)
val cn_package : class_name -> string list

(** Compares two classes names. *)
val cn_compare : class_name -> class_name -> int

(** Returns [true] if two classes names are equal, [false] otherwise. *)
val cn_equal : class_name -> class_name -> bool

(** Creating and manipulating {!method_signature} values. *)

(** [<clinit>] method signature. *)
val clinit_signature : method_signature

(** Builds a [method_signature]. *)
val make_ms : string -> value_type list -> value_type option -> method_signature

(** Returns the hash value of the given [method_signature]. *)
val ms_hash : method_signature -> int

(** Retrieves the method name from a [method_signature]. *)
val ms_name : method_signature -> string

(** Retrieves method parameters types from a [method_signature]. *)
val ms_args : method_signature -> value_type list

(** Retrieves method return type from a [method_signature]. *)
val ms_rtype : method_signature -> value_type option

(** Compares two method signatures. *)
val ms_compare : method_signature -> method_signature -> int

(** Returns [true] if two method signatures are equal, [false] otherwise. *)
val ms_equal : method_signature -> method_signature -> bool

(** Creating and manipulating {!field_signature} values. *)

(** Builds a [field_signature]. *)
val make_fs : string -> value_type -> field_signature

(** Returns the hash value of the given [field_signature]. *)
val fs_hash : field_signature -> int

(** Retrieves the field name from a [field_signature]. *)
val fs_name : field_signature -> string

(** Retrieves the field type from a [field_signature]. *)
val fs_type : field_signature -> value_type

(** Compares two field signatures. *)
val fs_compare : field_signature -> field_signature -> int

(** Returns [true] if two field signatures are equal, [false] otherwise. *)
val fs_equal : field_signature -> field_signature -> bool

(** Creating and manipulating {!class_field_signature} values. *)

(** Builds a [class_field_signature]. *)
val make_cfs : class_name -> field_signature -> class_field_signature

(** Retrieves the [class_name] and [field_signature] from a [class_field_signature]. *)
val cfs_split : class_field_signature -> class_name * field_signature

(** Compares two [class_field_signature]. *)
val cfs_compare : class_field_signature -> class_field_signature -> int

(** Returns [true] if two [class_field_signature] are equal, [false] otherwise. *)
val cfs_equal : class_field_signature -> class_field_signature -> bool

(** Creating and manipulating {!class_method_signature} values. *)

(** Builds a [class_method_signature]. *)
val make_cms : class_name -> method_signature -> class_method_signature

(** Retrieves the [class_name] and [method_signature] from a [class_method_signature]. *)
val cms_split : class_method_signature -> class_name * method_signature

(** Compares two [class_method_signature]. *)
val cms_compare : class_method_signature -> class_method_signature -> int

(** Returns [true] if two [class_method_signature] are equal, [false] otherwise. *)
val cms_equal : class_method_signature -> class_method_signature -> bool


(** {2 Constant pool.} *)

(** You should not need this for normal usage, as the
    parsing/unparsing functions take care of the constant pool. This
    is typically useful for user-defined attributes that refer to the
    constant pool. *)

(** Method descriptor. *)
type method_descriptor = value_type list * value_type option

(** Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor =
  | SValue of value_type
  | SMethod of method_descriptor

(** Abstract datatype for Java strings *)
type jstr
val make_jstr : string -> jstr
val jstr_pp   : jstr -> string
val jstr_raw  : jstr -> string

(** Constant value. *)
type constant_value =
  | ConstString of jstr
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type

(** Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * field_signature)
  | ConstMethod of (object_type * method_signature)
  | ConstInterfaceMethod of (class_name * method_signature)
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstUnusable


(** {2 Stackmaps}  *)

(** Verification type. *)
type verification_type =
  | VTop
  | VInteger
  | VFloat
  | VDouble
  | VLong
  | VNull
  | VUninitializedThis
  | VObject of object_type
  | VUninitialized of int (** creation point *)

(** Stackmap type. *)
type stackmap = (int* verification_type list * verification_type list)

(** {2 Errors}  *)

(** The library may throw the following exceptions, in addition to [Invalid_argument].
    Any other exception (in particular, an [Assert_failure])
    should be interpreted as a bug in [javalib]. *)

(** Indicates that a class name could not be found in a given classpath. *)
exception No_class_found of string

(** Indicates the argument of a parsing/unparsing function does not
    satisfy a structural constraint of class files. *)
exception Class_structure_error of string


(** {2 Annotations} *)

(** [element_value] represents a constant value, either a number, a string, a
    class, an enum, an array of [element_value]s or another annotation. *)
type element_value =
  | EVCstByte of int
  | EVCstChar of int
  | EVCstInt of int32
  | EVCstShort of int
  | EVCstBoolean of int
  | EVCstDouble of float
  | EVCstFloat of float
  | EVCstLong of int64
  | EVCstString of string
  | EVEnum of (class_name * string)
      (* (type_name_index,const_name_index) cf. JLS 13.1 *)
      (* TODO: this should probably be modified but I have not understand how *)
  | EVClass of value_type option
  | EVAnnotation of annotation
  | EVArray of element_value list

(** An [annotation] contains the name ([kind]) of the annotation an a list of
    element-value pairs (the name of the element and its value).  The names
    given here should corresponds to the elements declared during the definition
    of the annotation, but this is not checked (as it would need to load
    additional class files). *)
and annotation = {
  kind : class_name;
  element_value_pairs : (string * element_value) list;
}


(** {2 Containers.} *)

(** Common signature of set modules based on the Ptrees library. *)
module type GenericSetSig =
sig
  type t
  type elt
  val empty : t
  val is_empty : t -> bool
  val singleton : elt -> t
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val equal : t -> t -> bool
  val elements : t -> elt list
  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val inter : t -> t -> t
  val of_list : elt list -> t
  val of_array : elt array -> t
  (* val partition : (elt -> bool) -> t -> t * t *)
  (* val choose_and_remove : t -> elt * t *)
end

(** Common signature of map modules based on Ptrees library. *)
module type GenericMapSig =
sig
  type 'a t
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val modify: key -> ('a option -> 'a) -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (** [merge f m1 m2] returns a map that has the bindings of [m1] and [m2] and
        which binds [k] to [f d1 d2] if [m1] and [m2] binds the same [k] to
        different [d1] and [d2], respectively. If [d1] equals [d2], [f d1 d2] is
        supposed to return [d1]. *)
  val choose_and_remove : 'a t -> key * 'a * 'a t
    (** [choose_and_remove t] returns (i,d,t') such that [t'] equals to [remove
        i t] and [d] equals to [find i t].

        @raise Not_found if [t] is empty. *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val key_elements : 'a t -> key list
  val value_elements : 'a t -> 'a list
  val elements : 'a t -> (key * 'a) list
end

(** This module allows to build maps of elements indexed by [class_name] values. *)
module ClassMap : GenericMapSig with type key = class_name

(** This module allows to build maps of elements indexed by [method_signature] values. *)
module MethodMap : GenericMapSig with type key = method_signature

(** This module allows to build maps of elements indexed by [field_signature] values. *)
module FieldMap : GenericMapSig with type key = field_signature

(** This module allows to build maps of elements indexed by [class_field_signature] values. *)
module ClassFieldMap : GenericMapSig with type key = class_field_signature

(** This module allows to build maps of elements indexed by [class_method_signature] values. *)
module ClassMethodMap : GenericMapSig with type key = class_method_signature

(** This module allows to build sets of [class_name] values. *)
module ClassSet : GenericSetSig with type elt = class_name

(** This module allows to build sets of [method_signature] values. *)
module MethodSet : GenericSetSig with type elt = method_signature

(** This module allows to build sets of [field_signature] values. *)
module FieldSet : GenericSetSig with type elt = field_signature

(** This module allows to build sets of [class_field_signature] values. *)
module ClassFieldSet : GenericSetSig with type elt = class_field_signature

(** This module allows to build sets of [class_method_signature] values. *)
module ClassMethodSet : GenericSetSig with type elt = class_method_signature

module ClassMethodMaptoSet :
sig
  val to_set : 'a ClassMethodMap.t -> ClassMethodSet.t
end

(** {2 Tuning JavaLib.} *)

(** [set_permissive true] disables some checking in JavaLib.  It can
    allow to parse some files that do not strictly comply with the
    official specification.  *)
val set_permissive : bool -> unit

val get_permissive : unit -> bool

