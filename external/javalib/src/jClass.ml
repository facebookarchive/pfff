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

(* This implementation is only to provide MapFieldSignature and MapMethodSignature.*)

open JBasics
open JCode

(* Visibility modifiers. *)
type access = [
| `Default
| `Public
| `Private
| `Protected
]

(* Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  deprecated : bool;
  other : (string * string) list
}

type visibility = RTVisible | RTInvisible

type method_annotations = {
  ma_global: (annotation*visibility) list;
  ma_parameters: (annotation*visibility) list list;
}

(* {2 Fields of classes and interfaces.} *)
(*******************************)

type field_kind =
  | NotFinal
  | Final
  | Volatile

type class_field = {
  cf_signature : field_signature;
  cf_class_signature : class_field_signature;
  cf_generic_signature : JSignature.fieldTypeSignature option;
  cf_access: access;
  cf_static : bool;
  cf_synthetic : bool;
  cf_enum : bool;
  cf_kind : field_kind;
  cf_value : constant_value option; (* Only if the field is static final. *)
  cf_transient : bool;
  cf_annotations : (annotation*visibility) list;
  cf_other_flags : int list;
  cf_attributes : attributes
}

(* Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_signature : field_signature;
  if_class_signature : class_field_signature;
  if_generic_signature : JSignature.fieldTypeSignature option;
  if_synthetic : bool;
  if_value : constant_value option; (* a constant_value is not mandatory, especially as it can be initialized by the class initializer <clinit>. *)
  if_annotations : (annotation*visibility) list;
  if_other_flags : int list;
  if_attributes : attributes
}

type any_field =
    | InterfaceField of interface_field
    | ClassField of class_field

(* {2 Methods of classes and interfaces.} *)
(******************************************)

type 'a implementation =
  | Native
  | Java of 'a Lazy.t


(* l'attribut final n'a pas vraiment de sens pour une méthode
   statique, mais c'est autorisé dans la spec JVM. *)
type 'a concrete_method = {
  cm_signature : method_signature;
  cm_class_method_signature : class_method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool;
  cm_access: access;
  cm_generic_signature : JSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_annotations : method_annotations;
  cm_implementation : 'a implementation;
}

type abstract_method = {
  am_signature : method_signature;
  am_class_method_signature : class_method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : JSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  am_annotations : method_annotations;
  am_annotation_default : element_value option;
}


(* {2 Classes and interfaces.} *)
(***************************)

type 'a jmethod =
    | AbstractMethod of abstract_method
    | ConcreteMethod of 'a concrete_method

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_synthetic: bool;
  ic_annotation: bool;
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
  c_consts : constant array; (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  c_source_debug_extention : string option;
  c_inner_classes : inner_class list;
  c_synthetic: bool;
  c_enum: bool;
  c_annotations: (annotation*visibility) list;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : 'a jmethod MethodMap.t;
}

(* Interfaces cannot be final and can only contains abstract
    methods. Their super class is [java.lang.Object].*)
type 'a jinterface = {
  i_name : class_name;
  i_version : version;
  i_access : [`Public | `Default];
  i_interfaces : class_name list;
  i_generic_signature : JSignature.classSignature option;
  i_consts : constant array; (* needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  i_inner_classes : inner_class list;
  i_initializer : 'a concrete_method option; (* should be static/ signature is <clinit>()V; *)
  i_annotation: bool;
  i_annotations: (annotation*visibility) list;
  i_other_attributes : (string * string) list;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
}

type 'a interface_or_class =
  | JInterface of 'a jinterface
  | JClass of 'a jclass

let get_name = function
  | JInterface i -> i.i_name
  | JClass c -> c.c_name

let get_consts = function
  | JInterface i -> i.i_consts
  | JClass c -> c.c_consts

let get_access = function
  | JInterface i -> i.i_access
  | JClass c -> c.c_access

let get_sourcefile = function
  | JInterface i -> i.i_sourcefile
  | JClass c -> c.c_sourcefile

let is_deprecated = function
  | JInterface i -> i.i_deprecated
  | JClass c -> c.c_deprecated

let is_final = function
  | JInterface _ -> false
  | JClass c -> c.c_final

let get_inner_classes = function
  | JInterface i -> i.i_inner_classes
  | JClass c -> c.c_inner_classes

let get_other_attributes = function
  | JInterface i -> i.i_other_attributes
  | JClass c -> c.c_other_attributes

let get_initializer = function
  | JInterface i -> i.i_initializer
  | JClass c ->
      try
	match
	  MethodMap.find
	    clinit_signature
	    c.c_methods
	with
	  | ConcreteMethod m -> Some m
	  | AbstractMethod _ -> raise (Class_structure_error "A class initializer cannot be abstract")
      with
	| Not_found -> None

let get_other_flags = function
  | JInterface i -> i.i_other_flags
  | JClass c -> c.c_other_flags

let get_method ioc ms =
  match ioc with
    | JInterface {i_initializer = Some cm; _}
	when (ms_equal cm.cm_signature ms) ->
	ConcreteMethod cm
    | JInterface i ->
	AbstractMethod(MethodMap.find ms i.i_methods)
    | JClass c ->
	MethodMap.find ms c.c_methods

let get_concrete_method ioc ms =
  match ioc with
    | JInterface {i_initializer = Some cm; _}
	when (ms_equal cm.cm_signature ms) -> cm
    | JInterface _ -> raise Not_found
    | JClass c -> 
	(match MethodMap.find ms c.c_methods with
	    ConcreteMethod cm -> cm
	  | AbstractMethod _ -> raise Not_found)

let get_methods = function
  | JInterface i ->
      let mmap =
	MethodMap.map (fun am -> AbstractMethod am) i.i_methods in
	(match i.i_initializer with
	   | None -> mmap
	   | Some cm -> MethodMap.add clinit_signature (ConcreteMethod cm) mmap
	)
  | JClass {c_methods = mmap; _} -> mmap

let get_concrete_methods = function
  | JInterface i ->
      (match i.i_initializer with
	 | None -> MethodMap.empty
	 | Some cm -> MethodMap.add clinit_signature cm MethodMap.empty
      )
  | JClass {c_methods = mmap;_} ->
      MethodMap.fold
	(fun ms m mmap ->
	   match m with
	     | AbstractMethod _ -> mmap
	     | ConcreteMethod cm ->
		 MethodMap.add ms cm mmap
	) mmap MethodMap.empty

let get_field c fs = match c with
  | JInterface i ->
      InterfaceField (FieldMap.find fs i.i_fields)
  | JClass c -> ClassField (FieldMap.find fs c.c_fields)

let get_fields = function
  | JInterface i ->
      FieldMap.map (fun f -> InterfaceField f) i.i_fields
  | JClass c ->
      FieldMap.map (fun f -> ClassField f) c.c_fields

let defines_method ioc ms =
  match ioc with
    | JInterface i ->
	if (ms_equal ms clinit_signature) then
	  i.i_initializer <> None
	else MethodMap.mem ms i.i_methods
    | JClass c -> MethodMap.mem ms c.c_methods

let defines_field ioc fs =
  match ioc with
    | JInterface {i_fields = fm;_} ->
	FieldMap.mem fs fm
    | JClass {c_fields = fm;_} ->
	FieldMap.mem fs fm

let is_static_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_static

let is_final_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_final

let is_synchronized_method = function
  | AbstractMethod _ -> false
  | ConcreteMethod m -> m.cm_synchronized

let get_method_visibility = function
  | AbstractMethod m -> (m.am_access:>access)
  | ConcreteMethod m -> m.cm_access

let get_method_signature = function
  | AbstractMethod m -> m.am_signature
  | ConcreteMethod m -> m.cm_signature

let get_class_method_signature = function
  | AbstractMethod m -> m.am_class_method_signature
  | ConcreteMethod m -> m.cm_class_method_signature

let get_field_signature = function
  | InterfaceField {if_signature = fs;_}
  | ClassField {cf_signature = fs;_}
    -> fs

let is_final_field = function
  | InterfaceField _ -> true
  | ClassField f -> f.cf_kind = Final

let is_static_field = function
  | InterfaceField _ -> true
  | ClassField f -> f.cf_static

let get_field_visibility = function
  | ClassField f -> f.cf_access
  | InterfaceField _ -> `Public

let get_class_field_signature = function
  | InterfaceField {if_class_signature = cfs;_}
  | ClassField {cf_class_signature = cfs;_}
    -> cfs

(* let iter_methods f = function *)
(*   | JInterface i -> *)
(*       (match i.i_initializer with *)
(* 	 | Some i -> f i.cm_signature (ConcreteMethod i) *)
(* 	 | None -> ()) *)
(*   | JClass c -> MethodMap.iter f c.c_methods *)

(* let iter_concrete_methods f = function *)
(*   | JInterface i -> *)
(*       (match i.i_initializer with *)
(* 	 | Some i -> f i.cm_signature i *)
(* 	 | None -> ()) *)
(*   | JClass c -> *)
(*       MethodMap.iter *)
(* 	(fun s m -> match m with ConcreteMethod m -> f s m | AbstractMethod _ -> ()) *)
(* 	c.c_methods *)

(* let iter_fields f = function *)
(*   | JInterface i -> *)
(*       FieldMap.iter *)
(* 	(fun s fi -> f s (InterfaceField fi )) *)
(* 	i.i_fields *)
(*   | JClass c -> *)
(*       FieldMap.iter *)
(* 	(fun s fi -> f s (ClassField fi )) *)
(* 	c.c_fields *)

let identity = fun x -> x

let cf_iter f = function
  | JInterface _ -> ()
  | JClass c -> FieldMap.iter (fun _ fi -> f fi) c.c_fields
let if_iter f = function
  | JInterface i -> FieldMap.iter (fun _ fi -> f fi) i.i_fields
  | JClass _ -> ()
let f_iter f = function
  | JInterface i -> FieldMap.iter (fun _ fi -> f (InterfaceField fi)) i.i_fields
  | JClass c -> FieldMap.iter (fun _ fi -> f (ClassField fi)) c.c_fields


let f_fold f = function
  | JInterface i -> FieldMap.fold (fun _ fi v -> f (InterfaceField fi) v) i.i_fields
  | JClass c -> FieldMap.fold (fun _ fi v -> f (ClassField fi) v) c.c_fields
let if_fold f = function
  | JInterface i -> FieldMap.fold (fun _ fi v -> f fi v) i.i_fields
  | JClass _ -> identity
let cf_fold f = function
  | JInterface _ -> identity
  | JClass c -> FieldMap.fold (fun _ fi v -> f fi v) c.c_fields


let m_iter f = function
  | JInterface i ->
      Option.may (fun m -> f (ConcreteMethod m)) i.i_initializer;
      MethodMap.iter
        (fun _ m -> f (AbstractMethod m))
        i.i_methods
  | JClass c ->
      MethodMap.iter
        (fun _ m -> f m)
        c.c_methods

let cm_iter f = function
  | JInterface i -> Option.may (fun m -> f m) i.i_initializer;
  | JClass c ->
      MethodMap.iter
        (fun _ -> function
           | AbstractMethod _ -> ()
           | ConcreteMethod m -> f m)
        c.c_methods

let am_iter f = function
  | JInterface i -> MethodMap.iter (fun _ m -> f m) i.i_methods
  | JClass c ->
      MethodMap.iter
        (fun _ -> function
           | AbstractMethod m -> f m
           | ConcreteMethod _ -> ())
        c.c_methods


let am_fold f = function
  | JInterface i ->
      MethodMap.fold
        (fun _ m acc -> f m acc)
        i.i_methods
  | JClass c ->
      MethodMap.fold
        (fun _ -> function
           | AbstractMethod m -> f m
           | ConcreteMethod _ -> (fun acc -> acc))
        c.c_methods

let cm_fold f = function
  | JInterface {i_initializer = Some m;_} -> f m
  | JInterface _ -> identity
  | JClass c ->
      MethodMap.fold
        (fun _ -> function
           | AbstractMethod _ -> (fun acc -> acc)
           | ConcreteMethod m -> f m)
        c.c_methods

let m_fold f = function
  | JInterface i ->
      (fun d ->
         MethodMap.fold
           (fun _ m acc -> f (AbstractMethod m) acc)
           i.i_methods
           (Option.map_default
              (fun m -> f (ConcreteMethod m) d)
              d
              i.i_initializer))
  | JClass c ->
      MethodMap.fold
        (fun _ m acc -> f m acc)
        c.c_methods

(* val cm_fold : ('b -> 'a concrete_method -> 'b) -> 'b -> 'a node -> 'b *)
(* val am_fold : ('b -> 'a abstract_method -> 'b) -> 'b -> 'a node -> 'b *)
(* val m_fold : ('b -> 'a jmethod -> 'b) -> 'b -> 'a node -> 'b *)

let map_concrete_method_with_native f cm =
  {
    cm_signature = cm.cm_signature;
    cm_class_method_signature = cm.cm_class_method_signature;
    cm_static = cm.cm_static;
    cm_final = cm.cm_final;
    cm_synchronized = cm.cm_synchronized;
    cm_strict = cm.cm_strict;
    cm_access = cm.cm_access;
    cm_generic_signature = cm.cm_generic_signature;
    cm_bridge = cm.cm_bridge;
    cm_varargs = cm.cm_varargs;
    cm_synthetic = cm.cm_synthetic;
    cm_other_flags = cm.cm_other_flags;
    cm_exceptions = cm.cm_exceptions;
    cm_attributes = cm.cm_attributes;
    cm_annotations = cm.cm_annotations;
    cm_implementation = f cm.cm_implementation
  }

let map_concrete_method ?(force=false) f =
  map_concrete_method_with_native 
    (function
       | Native -> Native
       | Java c -> 
	   if force then
	     let new_c = f (Lazy.force c) in
	       Java (lazy new_c)
	   else Java (lazy (f (Lazy.force c))))

let map_concrete_method_with_native_context f cm = 
  map_concrete_method_with_native (f cm) cm

let map_concrete_method_context ?(force=false) f cm = 
  map_concrete_method ~force:force  (f cm) cm

let map_method ?(force=false) f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> ConcreteMethod (map_concrete_method ~force:force f cm)

let map_method_context ?(force=false) f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> ConcreteMethod (map_concrete_method_context ~force:force f cm)

let map_method_with_native f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> ConcreteMethod (map_concrete_method_with_native f cm)

let map_method_with_native_context f = function
  | AbstractMethod am -> AbstractMethod am
  | ConcreteMethod cm -> 
      ConcreteMethod (map_concrete_method_with_native_context f cm)

let map_class_gen map_method f c =
  {
    c_name = c.c_name;
    c_version = c.c_version;
    c_access = c.c_access;
    c_final = c.c_final;
    c_abstract = c.c_abstract;
    c_super_class = c.c_super_class;
    c_generic_signature = c.c_generic_signature;
    c_fields = c.c_fields;
    c_interfaces = c.c_interfaces;
    c_consts = c.c_consts;
    c_sourcefile = c.c_sourcefile;
    c_deprecated = c.c_deprecated;
    c_enclosing_method = c.c_enclosing_method;
    c_source_debug_extention = c.c_source_debug_extention;
    c_inner_classes = c.c_inner_classes;
    c_synthetic = c.c_synthetic;
    c_enum = c.c_enum;
    c_annotations = c.c_annotations;
    c_other_flags = c.c_other_flags;
    c_other_attributes = c.c_other_attributes;
    c_methods = MethodMap.map (map_method f) c.c_methods;
  }

let map_class_with_native_context f c = 
  map_class_gen map_method_with_native_context f c
let map_class_with_native f c  = 
  map_class_gen map_method_with_native f c

let map_class_context ?(force=false) f c = map_class_gen (map_method_context ~force:force) f c
let map_class ?(force=false) f c = map_class_gen (map_method ~force:force) f c

let map_interface_gen map_concrete_method f i =
  {
    i_name = i.i_name;
    i_version = i.i_version;
    i_access = i.i_access;
    i_interfaces = i.i_interfaces;
    i_generic_signature = i.i_generic_signature;
    i_consts = i.i_consts;
    i_sourcefile = i.i_sourcefile;
    i_deprecated = i.i_deprecated;
    i_source_debug_extention = i.i_source_debug_extention;
    i_inner_classes = i.i_inner_classes;
    i_other_attributes = i.i_other_attributes;
    i_initializer = (match i.i_initializer with
		       | None -> None
		       | Some cm ->
			   Some (map_concrete_method f cm)
		    );
    i_annotation = i.i_annotation;
    i_annotations = i.i_annotations;
    i_other_flags = i.i_other_flags;
    i_fields = i.i_fields;
    i_methods = i.i_methods;
  }

let map_interface_context ?(force=false) f i =
  map_interface_gen (map_concrete_method_context ~force:force) f i
let map_interface ?(force=false) f i = 
  map_interface_gen (map_concrete_method ~force:force) f i

let map_interface_with_native_context f i = 
  map_interface_gen map_concrete_method_with_native_context f i
let map_interface_with_native f i  = 
  map_interface_gen map_concrete_method_with_native f i

let map_interface_or_class_with_native f = function
  | JInterface i -> JInterface (map_interface_with_native f i)
  | JClass c -> JClass (map_class_with_native f c)

let map_interface_or_class_with_native_context f = function
  | JInterface i -> JInterface (map_interface_with_native_context f i)
  | JClass c -> JClass (map_class_with_native_context f c)

let map_interface_or_class ?(force=false) f = function
  | JInterface i -> JInterface (map_interface ~force f i)
  | JClass c -> JClass (map_class ~force f c)

let map_interface_or_class_context ?(force=false) f = function
  | JInterface i -> JInterface (map_interface_context ~force:force f i)
  | JClass c -> JClass (map_class_context ~force:force f c)
