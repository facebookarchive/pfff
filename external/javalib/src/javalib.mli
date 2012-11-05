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

(* TODO: add documentation on exception that can be raised by function.  *)

(** User interface for using the Javalib. *)

open JBasics
open JCode

(** {2 Common types.} *)

(** Visibility modifiers. *)
type access = [
| `Default
| `Public
| `Private
| `Protected
]

(** Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  (** correspond to the attribute, not to the flag (cf. JVM Spec 1.5
      §4.2, §4.6, §4.7 and §4.8.7) *)
  deprecated : bool;
  other : (string * string) list
}

(** visibility modifiers for annotations. An annotation may either be visible at
    run-time ([RTVisible]) or only present in the class file without being
    visible at run-time ([RTInvisible]).  (Note that there exists a third
    visibility at the Java source level, but as it corresponds to the
    source-only visibility they are not in the class file anymore.)  *)
type visibility = RTVisible | RTInvisible


(** {2 Fields of classes and interfaces.} *)

(** Field kind *)
type field_kind =
  | NotFinal
  | Final
  | Volatile


(** Fields of classes. *)
type class_field = {
  cf_signature : field_signature;
  cf_class_signature : class_field_signature;
  cf_generic_signature : JSignature.fieldTypeSignature option;
  cf_access: access;
  cf_static : bool;
  cf_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
  cf_enum : bool;
  cf_kind : field_kind;
  cf_value : constant_value option;
  cf_transient : bool;
  cf_annotations : (JBasics.annotation * visibility) list;
  cf_other_flags : int list;
  cf_attributes : attributes
}

(** Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_signature : field_signature;
  if_class_signature : class_field_signature;
  if_generic_signature : JSignature.fieldTypeSignature option;
  if_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
  if_value : constant_value option;
  (** a constant_value is not mandatory, especially as it can be
      initialized by the class initializer <clinit>. *)
  if_annotations: (annotation*visibility) list;
  if_other_flags : int list;
  if_attributes : attributes
}

type any_field = | InterfaceField of interface_field | ClassField of class_field

(** {2 Fields access functions.} *)

val get_field_signature : any_field -> field_signature
val get_class_field_signature : any_field -> class_field_signature
val get_field_visibility : any_field -> access
val is_static_field : any_field -> bool
val is_final_field : any_field -> bool


(** {2 Methods of classes and interfaces.} *)

type 'a implementation =
  | Native
  | Java of 'a Lazy.t

type method_annotations = {
  ma_global: (annotation*visibility) list;
  (** annotations that are for the whole method. *)
  ma_parameters: (annotation*visibility) list list;
  (** [\[al1,al2\]] represents the annotations for the 2 parameters of the
      method, [al1] being the annotations for the first parameter and [al2] the
      annotations for the second parameter.  The length is smaller than the
      number of parameters of the method (excluding the receiver this).*)
}

(* The final attribute has no meaning for a static method, but the JVM spec
   authorizes it anyway... *)
(* TODO : mettre les champs inutiles à la fin. *)
type 'a concrete_method = {
  cm_signature : method_signature;
  cm_class_method_signature : class_method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool; (* Correspond to flag ACC_STRICT, which shows if we are in
                    FP-strict mod or not. *)
  cm_access: access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_annotations : method_annotations;
  cm_implementation : 'a implementation;
}

(** An abstract method cannot not be final, synchronized, strict or private. *)
type abstract_method = {
  am_signature : method_signature;
  am_class_method_signature : class_method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  am_annotations : method_annotations;
  am_annotation_default : element_value option;
  (** If the method is in an annotation interface, then [am_annotation_default]
      may contains a default value for this method (annotation element). *)
}

type 'a jmethod =
  | AbstractMethod of abstract_method
  | ConcreteMethod of 'a concrete_method

(** {2 Methods access functions.} *)

val get_method_signature : 'a jmethod -> method_signature
val get_class_method_signature : 'a jmethod -> class_method_signature
val get_method_visibility : 'a jmethod -> access
val is_static_method : 'a jmethod -> bool
val is_final_method : 'a jmethod -> bool
val is_synchronized_method : 'a jmethod -> bool

(** {2 Classes and interfaces.} *)

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_synthetic: bool;
  ic_annotation: bool;
  (** [true] if and only if the class is an annotation (it should in this case
      be an interface) *)
  ic_enum: bool;
  ic_other_flags : int list;
  ic_type : [`ConcreteClass | `Abstract | `Interface]
}

type 'a jclass = {
  c_name : class_name;
  c_version : version;
  c_access : [`Public | `Default];
  c_final : bool;
  c_abstract : bool;
  c_super_class : class_name option;
  c_generic_signature : JSignature.classSignature option;
  c_fields : class_field FieldMap.t;
  c_interfaces : class_name list;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  (** introduced with Java 5 for local classes (defined in methods'
      code). The first element is innermost class that encloses the
      declaration of the current class. The second element is the
      method that encose this class definition. cf
      {{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS},
      paragraph 4.8.6.*)
  c_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  c_inner_classes : inner_class list;
  c_synthetic: bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.2 and §4.8.7) *)
  c_enum: bool;
  c_annotations: (annotation*visibility) list;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : 'a jmethod MethodMap.t;
}

(** Interfaces cannot be final and can only contains abstract
    methods. Their super class is [java.lang.Object].*)
type 'a jinterface = {
  i_name : class_name;
  i_version : version;
  i_access : [`Public | `Default];
  i_interfaces : class_name list;
  i_generic_signature : JSignature.classSignature option;
  i_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  i_inner_classes : inner_class list;
  i_initializer : 'a concrete_method option;
  (** the signature is <clinit>()V; and the method should be static  *)
  i_annotation: bool;
  (** [true] if and only if the interface is an annotation. *)
  i_annotations: (annotation*visibility) list;
  i_other_attributes : (string * string) list;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
}

type 'a interface_or_class =
  | JInterface of 'a jinterface
  | JClass of 'a jclass

(** {2 Classes access functions.} *)

(** Returns the fully qualified name of a [class_name]. *)
val get_name : 'a interface_or_class -> class_name

(** Returns the constant pool of a class or interface. *)
val get_consts : 'a interface_or_class -> constant array

(** Returns the access type of a class or interface. *)
val get_access : 'a interface_or_class -> [`Default | `Public]

(** Returns the source file associated to the parsed bytecode used to build
    the given class or interface. *)
val get_sourcefile : 'a interface_or_class -> string option

(** Returns [true] if the use of the given interface or class is deprecated,
    [false] otherwise. *)
val is_deprecated : 'a interface_or_class -> bool

(** Returns [true] if the given interface or class is final, [false]
    otherwise. *)
val is_final : 'a interface_or_class -> bool

(** Returns the list of inner classes defined in the given class or interface. *)
val get_inner_classes : 'a interface_or_class -> inner_class list

(** Returns the initializer of the given class or interface, if defined. *)
val get_initializer : 'a interface_or_class -> 'a concrete_method option

val get_other_attributes : 'a interface_or_class -> (string * string) list

val get_other_flags : 'a interface_or_class -> int list

(** [defines_method ioc ms] returns [true] if [ioc] defines a method with a signature equal
    to [ms], [false] otherwise. *)
val defines_method : 'a interface_or_class -> method_signature -> bool

(** [get_method ioc ms] returns the method with the signature [ms] in the
    interface or class [ioc]. @raise Not_found if such a method can't be found. *)
val get_method : 'a interface_or_class -> method_signature -> 'a jmethod

(** [get_concrete_method ioc ms] returns the method with the signature
    [ms] in the interface or class [ioc]. @raise Not_found if such a
    method can't be found or if the method is not concrete. *)
val get_concrete_method : 'a interface_or_class -> method_signature -> 'a concrete_method

(** Returns the map of all the methods defined in the given class or interface. *)
val get_methods : 'a interface_or_class -> 'a jmethod MethodMap.t

(** Returns the map of all the concrete methods defined in the given class or interface. *)
val get_concrete_methods : 'a interface_or_class -> 'a concrete_method MethodMap.t

(** [defines_field ioc fs] returns [true] if [ioc] defines a field with a signature equal
    to [fs], [false] otherwise. *)
val defines_field : 'a interface_or_class -> field_signature -> bool

(** [get_field ioc fs] returns the field with the signature [fs] in the
    interface or class [ioc]. @raise Not_found if such a field can't be found. *)
val get_field : 'a interface_or_class -> field_signature -> any_field

(** Returns the map of all the fields defined in the given class or interface. *)
val get_fields : 'a interface_or_class -> any_field FieldMap.t

(** {2 Iterators}  *)

val cf_iter : (class_field -> unit) -> 'a interface_or_class -> unit
val if_iter : (interface_field -> unit) -> 'a interface_or_class -> unit
val f_iter : (any_field -> unit) -> 'a interface_or_class -> unit
val cf_fold : (class_field -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b
val if_fold : (interface_field -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b
val f_fold : (any_field -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b

val cm_iter : ('a concrete_method -> unit) -> 'a interface_or_class -> unit
val am_iter : (abstract_method -> unit) -> 'a interface_or_class -> unit
val m_iter : ('a jmethod -> unit) -> 'a interface_or_class -> unit
val cm_fold : ('a concrete_method -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b
val am_fold : (abstract_method -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b
val m_fold : ('a jmethod -> 'b -> 'b) -> 'a interface_or_class -> 'b -> 'b


(** {2 Transforming code representation.} *)

(** [map_concrete_method f cm] lazily applies [f] to the implementation of [cm]
    (if the method is native, it does nothing but changing the type).  The
    application is {b lazy}: [f] is not applied until [Lazy.force] is called on
    the implementation: be careful if you use side-effects. *)
val map_concrete_method :
  ?force:bool -> ('a -> 'b) -> 'a concrete_method -> 'b concrete_method

(** [map_interface_or_class f ioc] lazily applies [f] to all non-native method
    implementations of the interface or class [ioc].  The application of [f] is
    {b lazy} (cf {!Javalib.map_concrete_method}).  *)
val map_interface_or_class :
  ?force:bool -> ('a -> 'b) -> 'a interface_or_class -> 'b interface_or_class

(** [map_interface_or_class_context f ioc] lazily applies [f] to all non-native
    method implementations of the interface or class [ioc], giving the concrete
    method being transformed as argument to [f].  The application of [f] is {b
    lazy} (cf {!Javalib.map_concrete_method}).  *)
val map_interface_or_class_context :
  ?force:bool -> ('a concrete_method -> 'a -> 'b) -> 'a interface_or_class -> 'b interface_or_class

(** {3 Alternative transforming functions} *)

(** [map_concrete_method_with_native f cm] is equivalent to
    {!map_concrete_method} but allows to transform not only the code
    representation but also the implementation type. It provides a way
    to modify the nature of implementation to [Native] if the code
    could not be transformed or conversly to provide a generated code
    for [Native] implementation. *)
val map_concrete_method_with_native :
  ('a implementation -> 'b implementation) -> 'a concrete_method -> 'b concrete_method

(** [map_interface_or_class_with_native f ioc] is equivalent to
    {!map_interface_or_class} but allows to transform not
    only the code representation but also the implementation type. It
    provides a way to modify the nature of implementation to [Native]
    if the code could not be transformed or conversly to provide a
    generated code for [Native] implementation. *)
val map_interface_or_class_with_native :
  ('a implementation -> 'b implementation) -> 'a interface_or_class -> 'b interface_or_class

(** [map_interface_or_class_with_native_context f ioc] is equivalent to
    {!map_interface_or_class_context} but allows to transform not
    only the code representation but also the implementation type. It
    provides a way to modify the nature of implementation to [Native]
    if the code could not be transformed or conversly to provide a
    generated code for [Native] implementation. *)
val map_interface_or_class_with_native_context :
  ('a concrete_method -> 'a implementation -> 'b implementation) -> 
  'a interface_or_class -> 'b interface_or_class

(** {2 Files manipulations.} *)

(** The type of "compiled" class paths (jar (or zip) files are opened for efficiency). *)
type class_path


(** [class_path cp] opens a class path from the list [cp] of
    directories and jar (or zip) files separated by {!JFile.sep}.  jar
    (or zip) files in the given directories are also considered, but
    they are not looked for recursively.  If [cp] is empty([""]), then
    the current directory is used.  Note: the order matters: the
    search stops when a class file is found. Directories and jar (or
    zip) files are read in the given order. When several directories
    are given, the order of the jar (or zip) file inside those
    directory are unspecified, but the jar (or zip) file of the first
    directory will be read before the others.

    Note : the following works :
    {[try class_path (Sys.getenv "CLASSPATH")
with Not_found-> class_path ""]}*)
val class_path : string -> class_path

(** Closes a class path. *)
val close_class_path : class_path -> unit

(** Parses a single class. It takes as argument the class name built
    with {!JBasics.make_cn}.
    This function does not check that the name of the parsed class is the
    same as the argument class name.

    @raise JBasics.No_class_found if the given class name [cn] does not
    correspond to a class file that can be found in the given class path.

    @raise JBasics.Class_structure_error if the class file does not match the
    official specification (although it does not check the class file
    entirely).  *)
val get_class : class_path -> class_name -> jcode interface_or_class

(** [write_class outputdir c] writes the class [c] in the subdirectory of
    [outputdir] that correspond to the package name of [c].

    @raise Class_structure_error if an opcode cannot be encoded in the available
    place.  *)
val write_class : string -> JCode.jcode interface_or_class -> unit

(** [extract_class_name_from_file f] recovers a class name and a class
    path from the file [f]. @raise Sys_error if [f] is not a file. [f]
    must contain the [.class] extension. *)
val extract_class_name_from_file : string -> JBasics.class_name * string

(** {3 More advanced files manipulations.} *)


(** [iter ~debug:false f filename] applies the function successively the
    function [f] on each classes specified by [filename]. [filename] is either a
    valid class file, a valid jar (or zip) file, or a valid directory with jar
    (or zip) files inside.  The dirname of [filename] is used as classpath.  If
    [debug] is [true] then the number of classes parsed when given a .jar file or
    a directory is printed on the standard error output.  *)
val iter :
  ?debug:bool ->
  (JCode.jcode interface_or_class -> unit) -> string -> unit

(** Abstract type representing a list of directories. *)
type directories

(** [make_directories directories] returns an abstract [directories] type.  The
    string [directories] must be a list of files separated by ":" under Unix or
    ";" under Windows. Only directories are filtered. *)
val make_directories : string -> directories

(** The following functions search for class files in the following order :
    - If a name can be found in some directory, subsequent directories are
    ignored.
    - If a name is the name of an existing directory, then every
    .class file inside this directory is read, and the search is over
    (even if the directory is empty).
    - Otherwise, if the name refers to an existing .class file
    (without the extension) then this file is read.
    - Otherwise, if the name ends in .jar or .zip and the file exists, it is
    assumed to be a jar file and the class files inside are read.

    Dots in class names are interpreted as / (but not for jar
    files). *)

(** [read directories f acc names] iterates [f] over all classes specified by
    [names]. [acc] is the initial accumulator value. *)
val read :
  directories -> ('a -> JCode.jcode interface_or_class -> 'a) -> 'a -> string list -> 'a

(** [transform directories outputdir f names] applies [f] to all classes
    specified by [names], writing the resulting classes in [outputdir]. Jar
    files are mapped to jar files, zip files to zip files and the non-class
    files are kept unchanged in the resulting archive. *)
val transform :
  directories -> string ->
  (JCode.jcode interface_or_class -> JCode.jcode interface_or_class) ->
  string list -> unit

(** {2 Dumping classes in .class format.} *)

(** [unparse_class ioc out] dump the class or interface [ioc] into [out] as a
    [.class] file.  The distance between the offset of two successive
    non-OpInvalid instructions is assumed to be at least the length of the
    optimal coding for the first instruction (for example, iload_0 vs iload
    0). The opcode may be encoded with the non-optimal form to fill the
    available space.

    @raise Class_structure_error if the length of an opcode produced is greater
    than the available space (number of OpInvalid + 1) except if
    {!JBasics.set_permissive} has been called with [true]. *)
val unparse_class : JCode.jcode interface_or_class -> out_channel -> unit


(** {2 Printing functions.} *)

(** Module gathering useful printing functions for basic types. *)
module JPrint :
sig
  (** {2 Printing basic signatures and descriptors.} *)

  (** Each of these functions gives a string representation of
      a type which has the corresponding name in JBasics.
      When set to true, the optional parameter [~jvm] allows to get a
      string representation following the JVM specification. Otherwise,
      a Java like representation (more readable) is given. *)

  (** [class_name ~jvm class_name] gives a string representation of a [class_name]. *)
  val class_name : ?jvm:bool -> class_name -> string

  (** Prints the package of the given [class_name]. *)
  val cn_package : class_name -> string

  (** Prints the name of the given [class_name], omitting the package name. *)
  val cn_simple_name : class_name -> string

  (** [method_signature ~jvm ~param_names method_signature] gives a
      string representation of a [method_signature]. The optional
      parameter [~param_name] can contain a list of the same length as
      the parameters.  In this case, when [~jvm] is set to false (or
      by default), the names of the parameters appear in the string
      representation of the method signature (like in Java).  The
      optional parameter [~callee] can contain an [object_type] which
      will be displayed in the signature as in the following example.
      {v void Object.toString() v}

      @raise Invalid_argument if the length of [param_names] (if given) is not
      the same as the number of arguments. *)
  val method_signature : ?jvm:bool ->
    ?callee:object_type ->
    ?param_names:string list -> method_signature -> string

  (** [class_method_signature ~jvm ~param_names cms] gives a string
      representation of a [class_method_signature] using
      {!Javalib.JPrint.method_signature} with the class name of [cms] as
      [callee].

      @raise Invalid_argument if the length of [param_names] (if given) is not
      the same as the number of arguments of the method.. *)
  val class_method_signature :
    ?jvm:bool -> ?param_names:string list -> class_method_signature -> string

  (** [field_signature ~jvm ~declared_in field_signature] gives a string
      representation of a [field_signature] where a class name may be given. *)
  val field_signature :
    ?jvm:bool -> ?declared_in:class_name -> field_signature -> string

  (** [class_field_signature ~jvm cfs] gives a string representation of a class
      field signature using {!Javalib.JPrint.field_signature}. *)
  val class_field_signature :
    ?jvm:bool -> class_field_signature -> string

  (** [signature ~jvm name descriptor] gives a string representation of a
      [descriptor] which can either contain a [field_descriptor] or a
      [method_descriptor], associated to the name [name]. *)
  val signature : ?jvm:bool -> string -> descriptor -> string

  (** [java_basic_type ~jvm java_basic_type] gives a string
      representation of a [java_basic_type]. *)
  val java_basic_type : ?jvm:bool -> java_basic_type -> string

  (** [object_type ~jvm object_type] gives a string representation of an
      [object_type]. *)
  val object_type : ?jvm:bool -> object_type -> string

  (** [value_type ~jvm value_type] gives a string representation of a
      [value_type]. *)
  val value_type : ?jvm:bool -> value_type -> string

  (** [return_type ~jvm rt] gives a string representation of the
      return type [rt]. The difference with [value_type] is that the added value
      [None] allows to represent the [void] value. *)
  val return_type : ?jvm:bool -> value_type option -> string

 (** [value_type_list ~names l] gives a string representation of the
     [value_type] list [l] with possibly associated [names].

     @raise Invalid_argument if the length of [names] is not the same as
     [l].  *)
  val value_type_list : ?jvm:bool -> ?names:string list -> value_type list -> string

  (** [field_descriptor ~jvm field_descriptor] gives a string representation of a
      [field_descriptor]. *)
  val field_descriptor : ?jvm:bool -> value_type -> string

  (** [method_descriptor ~jvm method_descriptor] gives a string representation of a
      method descriptor, composed of the parameters types and the return type of a
      method. *)
  val method_descriptor : ?jvm:bool -> value_type list -> value_type option -> string

  (** {2 Printing stackmaps, constant pool values and exception handlers.} *)

  val stack_map : stackmap -> string

  val constant : constant -> string

  val constant_pool : constant array -> string

  val exception_handler : exception_handler -> string

  (** {2 Printing JVM opcodes.} *)

  val jopcode : ?jvm:bool -> jopcode -> string

  val jcode : ?jvm:bool -> jcode -> string list

  (** {2 Outputting methods and classes.} *)

  val print_method : ?jvm:bool -> 'a jmethod -> ('a -> string list) -> out_channel -> unit

  val print_method' : ?jvm:bool -> 'a jmethod -> ('a -> Format.formatter -> unit) 
    -> Format.formatter -> unit

  val print_class : ?jvm:bool -> 'a interface_or_class -> ('a -> string list) -> out_channel -> unit

  val print_class' : ?jvm:bool -> 'a interface_or_class ->
    ('a -> Format.formatter -> unit) -> Format.formatter -> unit

  (** {2 Outputting classes in Jasmin format.} *)

  val print_jasmin : JCode.jcode interface_or_class -> out_channel -> unit

end
