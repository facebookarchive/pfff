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

(* $Id: gobject.ml 1524 2010-08-16 02:50:14Z garrigue $ *)

open StdLabels
open Gaux

type -'a obj
type g_type
type g_class
type g_value
type g_closure
type 'a objtype = g_type

type basic =
  [ `CHAR of char
  | `CAML of Obj.t
  | `BOOL of bool
  | `INT of int
  | `FLOAT of float
  | `STRING of string option
  | `POINTER of Gpointer.boxed option
  | `INT64 of int64 ]

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
    { kind: data_kind;
      proj: (data_conv_get -> 'a);
      inj: ('a -> unit data_set) }

type fundamental_type =
  [ `INVALID | `NONE | `INTERFACE | `PARAM | base_data ]

external do_unref : unit -> unit = "ml_g_object_do_unref"
let unref_alarm = Gc.create_alarm do_unref

module Type = struct
  external init : unit -> unit = "ml_g_type_init"
  let () = init ()
  external name : g_type -> string = "ml_g_type_name"
  external _from_name : string -> g_type = "ml_g_type_from_name"
  external parent : g_type -> g_type = "ml_g_type_parent"
  external depth : g_type -> int = "ml_g_type_depth"
  external is_a : g_type -> g_type -> bool = "ml_g_type_is_a"
  external fundamental : g_type -> fundamental_type
      = "ml_G_TYPE_FUNDAMENTAL"
  external of_fundamental : fundamental_type -> g_type
      = "ml_Fundamental_type_val"
  external interface_prerequisites : g_type -> g_type list
      = "ml_g_type_interface_prerequisites" (** @since GTK 2.2 *)
  external register_static : parent:g_type -> name:string -> g_type
      = "ml_g_type_register_static"
  let invalid =  of_fundamental `INVALID
  let from_name s =
    let t = _from_name s in
    if t = invalid then
      failwith ("Gobject.Type.from_name: " ^ s);
    t
  external g_caml_get_type : unit -> g_type = "ml_g_caml_get_type"
  let caml = g_caml_get_type ()
end

module Value = struct
  external create_empty : unit -> g_value = "ml_g_value_new"
      (* create a g_value owned by ML *)
  external init : g_value -> g_type -> unit = "ml_g_value_init"
  let create ty = let v = create_empty () in init v ty; v
      (* create and initialize a g_value *)
  external release : g_value -> unit = "ml_g_value_release"
      (* invalidate a g_value, releasing resources *)
  external get_type : g_value -> g_type = "ml_G_VALUE_TYPE"
  external copy : g_value -> g_value -> unit = "ml_g_value_copy"
  external reset : g_value -> unit = "ml_g_value_reset"
  external type_compatible : g_type -> g_type -> bool
      = "ml_g_value_type_compatible"
  external type_transformable : g_type -> g_type -> bool
      = "ml_g_value_type_transformable"
  external transform : g_value -> g_value -> bool = "ml_g_value_transform"
  external get : g_value -> data_get = "ml_g_value_get_mlvariant"
  external set : g_value -> 'a data_set -> unit = "ml_g_value_set_mlvariant"
  external get_pointer : g_value -> Gpointer.boxed = "ml_g_value_get_pointer"
  external get_nativeint : g_value -> nativeint = "ml_g_value_get_nativeint"
  external get_int32 : g_value -> int32 = "ml_g_value_get_int32"
  let get_conv kind v =
    try match kind with
    (* special case to get all 32 bits *)
    | `INT32 | `UINT32 -> `INT32 (get_int32 v)
    (* special case to avoid copy of boxed *)
    | `POINTER ->
        `POINTER (try Some (get_pointer v) with Gpointer.Null -> None)
    | _ -> (get v :> data_conv_get)
    with Failure ("Gobject.get_int32"|"Gobject.get_pointer") -> `NONE
end

module Closure = struct
  type args
  type argv = { result: g_value; nargs: int; args: args }

  external create : (argv -> unit) -> g_closure = "ml_g_closure_new"

  external _nth : args -> pos:int -> g_value = "ml_g_value_shift"
  let nth arg ~pos =
    if pos < 0 || pos >= arg.nargs then invalid_arg "Gobject.Closure.nth";
    _nth arg.args ~pos
  let result argv = argv.result
  let get_result_type arg = Value.get_type (result arg)
  let get_type arg ~pos = Value.get_type (nth arg ~pos)
  let get arg ~pos = Value.get (nth arg ~pos)
  let set_result arg = Value.set (result arg)
  let get_args arg =
    let rec loop args ~pos =
      if pos < 0 then args
      else loop (get arg ~pos :: args) ~pos:(pos-1)
    in loop [] ~pos:(arg.nargs - 1)

  let get_pointer arg ~pos = Value.get_pointer (nth arg ~pos)
  let get_nativeint arg ~pos = Value.get_nativeint (nth arg ~pos)
  let get_int32 arg ~pos = Value.get_int32 (nth arg ~pos)
end

let objtype_from_name ~caller name =
  let t = Type._from_name name in
  let f = Type.fundamental t in
  if f = `INVALID then
    failwith (caller ^ " : type " ^ name ^ " is not yet defined");
  if f <> `OBJECT then
    failwith (caller ^ " : " ^ name ^ " is not an object type");
  t

external get_type : 'a obj -> g_type = "ml_G_TYPE_FROM_INSTANCE"
external get_object_type : 'a obj -> g_type = "ml_G_TYPE_FROM_INSTANCE"
let is_a obj name =
  Type.is_a (get_type obj) (objtype_from_name ~caller:"Gobject.is_a" name)

exception Cannot_cast of string * string
external unsafe_cast : 'a obj -> 'b obj = "%identity"
let try_cast w name =
  if is_a w name then unsafe_cast w
  else raise (Cannot_cast(Type.name(get_type w), name))

external coerce : 'a obj -> unit obj = "%identity"
external coerce_option : 'a obj option -> unit obj option = "%identity"
  (* [coerce] is safe *)

external unsafe_create : g_type -> (string * 'a data_set) list -> 'b obj
    = "ml_g_object_new"
  (* This is dangerous! *)
external unsafe_unref : 'a obj -> unit = "ml_g_object_unref"
external get_ref_count : 'a obj -> int = "ml_g_object_ref_count"

type ('a,'b) property = { name: string; conv: 'b data_conv }

type 'a param = string * unit data_set
let dyn_param prop v =
  (prop, (Obj.magic (v : 'a data_set) : unit data_set))
let param (prop : ('a,'b) property) d : 'a param =
  dyn_param prop.name (prop.conv.inj d)

let unsafe_create ~classe l =
  unsafe_create (objtype_from_name ~caller:"Gobject.unsafe_create" classe) l

let get_oid (obj : 'a obj) : int = (snd (Obj.magic obj) lor 0)

module Data = struct
  let boolean =
    { kind = `BOOLEAN;
      proj = (function `BOOL b -> b | _ -> failwith "Gobject.get_bool");
      inj = (fun b -> `BOOL b) }
  let char =
    { kind = `CHAR;
      proj = (function `CHAR c -> c | _ -> failwith "Gobject.get_char");
      inj = (fun c -> `CHAR c) }
  let uchar = {char with kind = `UCHAR}
  let int =
    { kind = `INT;
      proj = (function `INT c -> c | _ -> failwith "Gobject.get_int");
      inj = (fun c -> `INT c) }
  let uint = {int with kind = `UINT}
  let long = {int with kind = `LONG}
  let ulong = {int with kind = `ULONG}
  let int32 =
    { kind = `INT32;
      proj = (function `INT32 c -> c | _ -> failwith "Gobject.get_int32");
      inj = (fun c -> `INT32 c) }
  let uint32 = {int32 with kind = `UINT32}
  let flags tbl =
    { kind = `FLAGS;
      proj = (function `INT c -> Gpointer.decode_flags tbl c
             | _ -> failwith "Gobject.get_flags");
      inj = (fun c -> `INT (Gpointer.encode_flags tbl c)) }
  let enum tbl =
    { kind = `ENUM;
      proj = (function `INT c -> Gpointer.decode_variant tbl c
             | _ -> failwith "Gobject.get_enum");
      inj = (fun c -> `INT (Gpointer.encode_variant tbl c)) }
  let int64 =
    { kind = `INT64;
      proj = (function `INT64 c -> c | _ -> failwith "Gobject.get_int64");
      inj = (fun c -> `INT64 c) }
  let uint64 = {int64 with kind = `UINT64}
  let float =
    { kind = `FLOAT;
      proj = (function `FLOAT c -> c | _ -> failwith "Gobject.get_float");
      inj = (fun c -> `FLOAT c) }
  let double = {float with kind = `DOUBLE}
  let string =
    { kind = `STRING;
      proj = (function `STRING (Some s) -> s | `STRING None -> ""
             | _ -> failwith "Gobject.get_string");
      inj = (fun s -> `STRING (Some s)) }
  let string_option =
    { kind = `STRING;
      proj = (function `STRING s -> s
             | _ -> failwith "Gobject.get_string_option");
      inj = (fun s -> `STRING s) }
  let pointer =
    { kind = `POINTER;
      proj = (function `POINTER c -> c | _ -> failwith "Gobject.get_pointer");
      inj = (fun c -> `POINTER c) }
  let unsafe_pointer =
    { kind = `POINTER;
      proj = (function `POINTER (Some c) -> Obj.magic c
             | _ -> failwith "Gobject.get_pointer");
      inj = (fun c -> `POINTER (Some (Obj.magic c))) }
  let magic : 'a option -> 'b option = Obj.magic
  let unsafe_pointer_option =
    { kind = `POINTER;
      proj = (function `POINTER c -> magic c
             | _ -> failwith "Gobject.get_pointer");
      inj = (fun c -> `POINTER (magic c)) }
  let boxed_type t =
    if Type.fundamental t <> `BOXED then failwith "Gobject.Data.boxed_type";
    `OTHER t
  let boxed t = {pointer with kind = boxed_type t}
  let unsafe_boxed t = {unsafe_pointer with kind = boxed_type t}
  let unsafe_boxed_option t = {unsafe_pointer_option with kind = boxed_type t}
  let gobject_option =
    { kind = `OBJECT;
      proj = (function `OBJECT c -> may_map ~f:unsafe_cast c
             | _ -> failwith "Gobject.get_object");
      inj = (fun c -> `OBJECT (may_map ~f:unsafe_cast c)) }
  let gobject =
    { kind = `OBJECT;
      proj = (function `OBJECT (Some c) -> unsafe_cast c
             | `OBJECT None -> raise Gpointer.Null
             | _ -> failwith "Gobject.get_object");
      inj = (fun c -> `OBJECT (Some (unsafe_cast c))) }
  let gobject_by_name name =
    { gobject with kind = `OTHER (Type.from_name name) }
  let caml =
    { kind = `OTHER Type.caml;
      proj = (function `CAML v -> Obj.obj v
             | _ -> failwith "Gobject.get_caml") ;
      inj = (fun v -> `CAML (Obj.repr v)) }
  let caml_option =
    { kind = `OTHER Type.caml;
      proj = (function `CAML v -> Some (Obj.obj v)
	     | `NONE -> None
             | _ -> failwith "Gobject.get_caml") ;
      inj = (function None -> `POINTER None | Some v -> `CAML (Obj.repr v)) }
  let wrap ~inj ~proj conv =
    { kind = conv.kind;
      proj = (fun x -> proj (conv.proj x));
      inj = (fun x -> conv.inj (inj x)) }

  let of_value conv v =
    conv.proj (Value.get_conv conv.kind v)
  let type_of_kind = function
    | `INT32 -> Type.of_fundamental `INT
    | `UINT32 -> Type.of_fundamental `UINT
    | `OTHER t -> t
    | #base_data as x -> Type.of_fundamental x
  let get_type conv = type_of_kind conv.kind
  let to_value conv x =
    let v = Value.create (get_type conv) in
    Value.set v (conv.inj x);
    v
end

module Property = struct
  external freeze_notify : 'a obj -> unit = "ml_g_object_freeze_notify"
  external thaw_notify : 'a obj -> unit = "ml_g_object_thaw_notify"
  external notify : 'a obj -> string -> unit = "ml_g_object_notify"
  external set_value : 'a obj -> string -> g_value -> unit 
    = "ml_g_object_set_property"
  external get_value : 'a obj -> string -> g_value -> unit
    = "ml_g_object_get_property"
  external get_type : 'a obj -> string -> g_type
    = "ml_my_g_object_get_property_type"
  (* [get_property_type o name] may raise [Invalid_argument name] *)
  (* Converted the following to C to avoid too many calls
  let set_dyn obj prop data =
    let t = get_type obj prop in
    let v = Value.create t in
    Value.set v data;
    set_value obj prop v
  let get_dyn obj prop =
    let t = get_type obj prop in
    let v = Value.create t in
    get_value obj prop v;
    Value.get v
  *)
  external set_dyn : 'a obj -> string -> 'b data_set -> unit
    = "ml_g_object_set_property_dyn"
  external get_dyn : 'a obj -> string -> data_get
    = "ml_g_object_get_property_dyn"
  let set (obj : 'a obj) (prop : ('a,_) property) x =
    set_dyn obj prop.name (prop.conv.inj x)
  let get (obj : 'a obj) (prop : ('a,_) property) =
    let v =
      match prop.conv.kind with
      (* Special cases: need to bypass normal conversion *)
      | `INT32 | `UINT32 | `POINTER as k ->
          let t = get_type obj prop.name in
          let v = Value.create t in
          get_value obj prop.name v;
          Value.get_conv k v
      | _ ->
          (get_dyn obj prop.name :> data_conv_get)
    in
    prop.conv.proj v

  let get_some obj prop =
    match get obj prop with Some x -> x
    | None -> failwith ("Gobject.Property.get_some: " ^ prop.name)

  let check obj prop =
    let tp obj = Type.name (get_object_type obj) in
    let _data =
      try get_dyn obj prop.name
      with
        Invalid_argument _ -> failwith (tp obj ^ " has no property " ^ prop.name)
      | exn ->
          prerr_endline
            ("exception while looking for " ^ tp obj ^ "->" ^ prop.name);
          raise exn
    in
    try ignore (get obj prop) with
      Failure s ->
        failwith (s ^ " cannot handle " ^ tp obj ^ "->" ^ prop.name)
    | exn ->
        failwith (tp obj ^ "->" ^ prop.name ^
                  " raised " ^ Printexc.to_string exn)

  let may_cons prop x l =
    match x with Some x -> param prop x :: l | None -> l
  let may_cons_opt prop x l =
    match x with Some _ -> param prop x :: l | None -> l
end

let set o p x = Property.set p o x
let get o p = Property.get p o
let set_params obj params =
  List.iter params ~f:(fun (prop,arg) -> Property.set_dyn obj prop arg)
