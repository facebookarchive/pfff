(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gobject.mli 1443 2009-01-20 09:49:48Z ben_99_9 $ *)

type -'a obj
type g_type
type g_class
type g_value
type g_closure
type basic =
  [ `BOOL of bool
  | `CAML of Obj.t
  | `CHAR of char
  | `FLOAT of float
  | `INT of int
  | `INT64 of int64
  | `POINTER of Gpointer.boxed option
  | `STRING of string option ]

type data_get = [ basic | `NONE | `OBJECT of unit obj option ]
type 'a data_set =
  [ basic | `OBJECT of 'a obj option | `INT32 of int32 | `LONG of nativeint ]

type base_data =
  [ `BOOLEAN
  | `CHAR
  | `UCHAR
  | `INT
  | `UINT
  | `LONG
  | `ULONG
  | `INT64
  | `UINT64
  | `ENUM
  | `FLAGS
  | `FLOAT
  | `DOUBLE
  | `STRING
  | `POINTER
  | `BOXED
  | `OBJECT ]

type data_kind = [ `INT32 | `UINT32 | `OTHER of g_type | base_data ]
type data_conv_get = [ `INT32 of int32 | data_get ]

type 'a data_conv =
    { kind : data_kind; proj : data_conv_get -> 'a; inj : 'a -> unit data_set }

type ('a, 'b) property = { name : string; conv : 'b data_conv }

type fundamental_type =
  [ `INVALID | `NONE | `INTERFACE | `PARAM | base_data ]

exception Cannot_cast of string * string

val get_type : 'a obj -> g_type
val is_a : 'a obj -> string -> bool
val try_cast : 'a obj -> string -> 'b obj
val get_oid : 'a obj -> int

external unsafe_cast : 'a obj -> 'b obj = "%identity"
external coerce : 'a obj -> unit obj = "%identity"
external coerce_option : 'a obj option -> unit obj option = "%identity"
    (* [coerce] and [coerce_option] are safe *)

type +'a param
val dyn_param : string -> 'a data_set -> 'b param
val param : ('a,'b) property -> 'b -> 'a param

val unsafe_create : classe:string -> 'a param list -> 'a obj
    (* This type is NOT safe *)
val unsafe_unref : 'a obj -> unit
    (* Creates a NULL pointer; many places do not check for them! *)
val get_ref_count : 'a obj -> int
    (* Number of references to an object (for debugging) *)

val set : ('a, 'b) property -> 'a obj -> 'b -> unit
  (* Will not raise an exception but may emit a Glib warning and
     ignore the property if it does not exist. *)
val get : ('a, 'b) property -> 'a obj -> 'b
  (* [get prop o] may raise [Invalid_argument prop_name] *)
val set_params : 'a obj -> 'a param list -> unit
  (* May emit a Glib warning and ignore the non existent properties. *)

module Type :
  sig
    val init : unit -> unit
    val name : g_type -> string
    val from_name : string -> g_type
    val parent : g_type -> g_type
    val depth : g_type -> int
    val is_a : g_type -> g_type -> bool
    val fundamental : g_type -> fundamental_type
    val of_fundamental : fundamental_type -> g_type
    val interface_prerequisites : g_type -> g_type list
      
      (* [Benjamin] Experimental stub: the new class has the same size as 
      its parent. No init functions right now. *)
    val register_static : parent:g_type -> name:string -> g_type

    val caml : g_type
  end

module Value :
  sig
    val create_empty : unit -> g_value
    val init : g_value -> g_type -> unit
    val create : g_type -> g_value
    val release : g_value -> unit
    val get_type : g_value -> g_type
    val copy : g_value -> g_value -> unit
    val reset : g_value -> unit
    val type_compatible : g_type -> g_type -> bool
    val type_transformable : g_type -> g_type -> bool
    val transform : g_value -> g_value -> bool
    val get : g_value -> data_get
    val set : g_value -> 'a data_set -> unit
    val get_pointer : g_value -> Gpointer.boxed
    val get_nativeint : g_value -> nativeint
    val get_int32 : g_value -> int32
    val get_conv : data_kind -> g_value -> data_conv_get
  end

module Closure :
  sig
    type args
    type argv = { result : g_value; nargs : int; args : args; }
    val create : (argv -> unit) -> g_closure
    val nth : argv -> pos:int -> g_value
    val result : argv -> g_value
    val get_result_type : argv -> g_type
    val get_type : argv -> pos:int -> g_type
    val get : argv -> pos:int -> data_get
    val set_result : argv -> 'a data_set -> unit
    val get_args : argv -> data_get list
    val get_pointer : argv -> pos:int -> Gpointer.boxed
    val get_nativeint : argv -> pos:int -> nativeint
    val get_int32 : argv -> pos:int -> int32
  end

module Data :
  sig
    val boolean : bool data_conv
    val char : char data_conv
    val uchar : char data_conv
    val int : int data_conv
    val uint : int data_conv
    val long : int data_conv
    val ulong : int data_conv
    val flags : ([>  ] as 'a) Gpointer.variant_table -> 'a list data_conv
    val enum : ([>  ] as 'a) Gpointer.variant_table -> 'a data_conv
    val int32 : int32 data_conv
    val uint32 : int32 data_conv
    val int64 : int64 data_conv
    val uint64 : int64 data_conv
    val float : float data_conv
    val double : float data_conv
    val string : string data_conv
    val string_option : string option data_conv
    (* pointers disable copy *)
    val pointer : Gpointer.boxed option data_conv
    val unsafe_pointer : 'a data_conv
    val unsafe_pointer_option : 'a option data_conv
    (* use boxed to enable copy of parameter *)
    val boxed : g_type -> Gpointer.boxed option data_conv
    val unsafe_boxed : g_type -> 'a data_conv
    val unsafe_boxed_option : g_type -> 'a option data_conv
    val gobject : 'a obj data_conv
    val gobject_option : 'a obj option data_conv
    val gobject_by_name : string -> 'a obj data_conv
    val caml : 'a data_conv
    val caml_option : 'a option data_conv
    val wrap :
      inj:('a -> 'b) -> proj:('b -> 'a) -> 'b data_conv -> 'a data_conv
    val of_value : 'a data_conv -> g_value -> 'a
    val to_value : 'a data_conv -> 'a -> g_value
    val get_type : 'a data_conv -> g_type
  end

module Property :
  sig
    val freeze_notify : 'a obj -> unit
    val thaw_notify : 'a obj -> unit
    val notify : 'a obj -> string -> unit

    val set_value : 'a obj -> string -> g_value -> unit
      (* [set_value o name] may raise [Invalid_argument name] *)
    val get_value : 'a obj -> string -> g_value -> unit
      (* [get_value o name] may raise [Invalid_argument name] *)
    val get_type : 'a obj -> string -> g_type
      (* [get_type o name] may raise [Invalid_argument name] *)

    val set_dyn : 'a obj -> string -> 'b data_set -> unit
      (* Will not raise an exception but may emit a Glib warning and
         ignore the property if it does not exist. *)
    val get_dyn : 'a obj -> string -> data_get
      (* [set_type o name] may raise [Invalid_argument name] *)
    val set : 'a obj -> ('a, 'b) property -> 'b -> unit
      (* Will not raise an exception but may emit a Glib warning and
         ignore the property if it does not exist. *)
    val get : 'a obj -> ('a, 'b) property -> 'b
      (* [get o prop] may raise [Invalid_argument prop_name] *)

    val get_some : 'a obj -> ('a, 'b option) property -> 'b
    val check : 'a obj -> ('a, 'b) property -> unit
    val may_cons :
      ('a,'b) property -> 'b option -> 'a param list -> 'a param list
    val may_cons_opt :
      ('a,'b option) property -> 'b option -> 'a param list -> 'a param list
  end

