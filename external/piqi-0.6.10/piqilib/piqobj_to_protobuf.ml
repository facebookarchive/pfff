(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


module C = Piqi_common
open C

open Piqobj_common


module W = Piqi_protobuf


(* whether to generate piqi-any in external mode, i.e. only including fields
 * defined for it by piqi.piqi (and not piqi-impl.piqi)
 *)
let is_external_mode = ref false


(* providing special handling for boxed objects, since they are not
 * actual references and can not be uniquely identified. Moreover they can
 * mask integers which are used for enumerating objects *)
let refer obj =
  let count = Piqloc.next_ocount () in
  if not (Obj.is_int (Obj.repr obj))
  then Piqloc.addref obj count
  else ()


let reference f code x =
  refer x;
  f code x


(* XXX: move to Piqi_protobuf? *)
let gen_int ?wire_type code x =
  let wire_type = W.get_wire_type `int wire_type in
  let gen_f =
    match wire_type with
      | `varint -> Piqirun.int64_to_varint
      | `zigzag_varint -> Piqirun.int64_to_zigzag_varint
      | `fixed32 -> Piqirun.int64_to_fixed32
      | `fixed64 -> Piqirun.int64_to_fixed64
      | `signed_varint -> Piqirun.int64_to_signed_varint
      | `signed_fixed32 -> Piqirun.int64_to_signed_fixed32
      | `signed_fixed64 -> Piqirun.int64_to_signed_fixed64
      | `block -> assert false (* XXX *)
  in
  gen_f code x


let gen_packed_int ?wire_type x =
  let wire_type = W.get_wire_type `int wire_type in
  let gen_f =
    match wire_type with
      | `varint -> Piqirun.int64_to_packed_varint
      | `zigzag_varint -> Piqirun.int64_to_packed_zigzag_varint
      | `fixed32 -> Piqirun.int64_to_packed_fixed32
      | `fixed64 -> Piqirun.int64_to_packed_fixed64
      | `signed_varint -> Piqirun.int64_to_packed_signed_varint
      | `signed_fixed32 -> Piqirun.int64_to_packed_signed_fixed32
      | `signed_fixed64 -> Piqirun.int64_to_packed_signed_fixed64
      | `block -> assert false (* XXX *)
  in
  gen_f x


let gen_float ?wire_type code x =
  let wire_type = W.get_wire_type `float wire_type in
  let gen_f =
    match wire_type with
      | `fixed32 -> Piqirun.float_to_fixed32
      | `fixed64 -> Piqirun.float_to_fixed64
      | _ -> assert false (* XXX *)
  in
  gen_f code x


let gen_packed_float ?wire_type x =
  let wire_type = W.get_wire_type `float wire_type in
  let gen_f =
    match wire_type with
      | `fixed32 -> Piqirun.float_to_packed_fixed32
      | `fixed64 -> Piqirun.float_to_packed_fixed64
      | _ -> assert false (* XXX *)
  in
  gen_f x


let gen_bool = Piqirun.gen_bool_field

let gen_packed_bool = Piqirun.bool_to_packed_varint

let gen_string = Piqirun.gen_string_field


let gen_int ?wire_type code x =
  refer x;
  gen_int ?wire_type code x

let gen_float ?wire_type code x =
  refer x;
  gen_float ?wire_type code x

let gen_bool = reference gen_bool
let gen_string = reference gen_string


let compare_field_type a b =
  match a.T.Field.code, b.T.Field.code with
    | Some a, Some b -> Int32.to_int (Int32.sub a b)
    | _ -> assert false


let compare_field a b =
  let open F in
  compare_field_type a.t b.t


(* preorder fields by their codes *)
let order_fields = List.sort compare_field


(*
let rec unalias (x:Piqobj.obj) =
  match x with
    | `alias x -> unalias x.A.obj
    | x -> x
*)


let rec gen_obj code (x:Piqobj.obj) =
  match x with
    (* built-in types *)
    | `int x | `uint x -> gen_int code x
    | `float x -> gen_float code x
    | `bool x -> gen_bool code x
    | `string x -> gen_string code x
    | `binary x -> gen_string code x
    | `any x -> gen_any code x
    (* custom types *)
    | `record x -> reference gen_record code x
    | `variant x -> reference gen_variant code x
    | `enum x -> reference gen_enum code x
    | `list x -> reference gen_list code x
    | `alias x -> gen_alias code x


and gen_packed_obj (x:Piqobj.obj) =
  match x with
    (* built-in types *)
    | `int x | `uint x -> gen_packed_int x
    | `float x -> gen_packed_float x
    | `bool x -> gen_packed_bool x
    | `enum x -> gen_packed_enum x
    | `alias x -> gen_packed_alias x
    | _ ->
        assert false (* other objects can't be packed *)


(* generate obj without leading code/tag element *)
and gen_binobj x =
  Piqirun.gen_binobj gen_obj x


(* generate "Piqi_piqi.any" record from Piqobj.any *)
and gen_any code x =
  let open Any in
  let piqi_any =
    if not !is_external_mode
    then
      (* in internal mode, passing a reference to intermediate Any
       * prepresentation registered using Piqi_objstore *)
      let res = T.Any.({
        (T.default_any ()) with
        ref = Some (Piqobj.put_any x);
      })
      in Piqloc.addrefret x res
    else
      (* in external mode, leave only fields defined by piqi.piqi: protobuf and
       * typename *)
      let typename = x.typename in
      let protobuf = Piqobj.pb_of_any x in
      (* if protobuf is undefined, see if we have untyped JSON or XML *)
      (* XXX, TODO: use unindented JSON and XML to preserve space *)
      let json =
        if protobuf <> None
        then None
        else
          match Piqobj.json_of_any x with
            | None -> None
            | Some json_ast ->
                let s = !Piqobj.string_of_json json_ast in
                Some s
      in
      let xml =
        if protobuf <> None || json <> None
        then None
        else
          match Piqobj.xml_of_any x with
            | None -> None
            | Some xml_elems ->
                let s = !Piqobj.string_of_xml (`Elem ("value", xml_elems)) in
                Some s
      in
      T.Any.({
        (T.default_any ()) with
        typename = typename;
        protobuf = protobuf;
        json = json;
        xml = xml;
      })
  in
  T.gen__any code piqi_any


and gen_record code x =
  let open R in
  (* TODO, XXX: doing ordering at every generation step is inefficient *)
  let fields = order_fields x.field in

  let encoded_piq_unparsed =
    match x.unparsed_piq_fields_ref with
      | Some x when not !is_external_mode ->
          let obj = Int64.of_int x in
          (* making Piqloc happy by adding a fake reference *)
          Piqloc.add_fake_loc obj ~label:"_unparsed_piq_fields_ref";

          let encoded_x = gen_int 1 obj ~wire_type:`varint in
          [encoded_x]
      | _ ->
          []
  in
  let encoded_fields = encoded_piq_unparsed @ (gen_fields fields) in
  Piqirun.gen_record code encoded_fields


and gen_fields fields =
  (* check if there's at least one packed field
   * TODO: optimize by keeping track of it statically at the record level *)
  if List.exists is_packed_field fields
  then group_gen_fields fields
  else List.map gen_field fields


and is_packed_field x = x.F.t.T.Field.protobuf_packed


(* generate fields but first group packed repeated fields together because they
 * are represented differently on the wire *)
and group_gen_fields fields =
  let rec aux accu l =
    match l with
      | [] -> List.rev accu
      | h::t ->
          let res, t =
            if is_packed_field h
            then gen_packed_fields h t
            else gen_field h, t
          in
          aux (res::accu) t
  in
  aux [] fields


(* group repeated packed fields that have the same type as the first one,
 * generate their wire representation and return when we encounter some other
 * field that doesn't belong to this repeated group *)
and gen_packed_fields first tail =
  let open F in
  let return accu rest =
    let code = Int32.to_int (some_of first.t.T.Field.code) in
    let res = Piqirun.gen_record code (List.rev accu) in
    res, rest
  in
  let rec aux accu l =
    match l with
      | [] -> return accu l
      | h::_ when h.t != first.t -> return accu l
      | h::t ->
          aux ((gen_packed_field h)::accu) t
  in
  (* putting `first` as a first element in our accumulator *)
  aux [gen_packed_field first] tail


and gen_packed_field x =
  let open F in
  (* NOTE: object for a repeated packed field can't be None *)
  gen_packed_obj (some_of x.obj)


and gen_field x =
  let open F in
  let code = Int32.to_int (some_of x.t.T.Field.code) in
  match x.obj with
    | None ->
        (* using true for encoding flags -- the same encoding as for options
         * (see below) *)
        refer x;
        Piqirun.gen_bool_field code true
    | Some obj -> gen_obj code obj


and gen_variant code x =
  let open V in
  (* generate a record with a single field which represents variant's option *)
  Piqirun.gen_record code [gen_option x.option]


and gen_option x =
  let open O in
  let code = Int32.to_int (some_of x.t.T.Option.code) in
  match x.obj with
    | None ->
        (* using true for encoding options w/o value *)
        refer x;
        Piqirun.gen_bool_field code true
    | Some obj ->
        gen_obj code obj


and gen_enum code x =
  let open E in
  gen_enum_option code x.option


and gen_enum_option code x =
  let open O in
  let value = some_of x.t.T.Option.code in
  Piqirun.int32_to_signed_varint code value


and gen_packed_enum x =
  let open E in
  gen_packed_enum_option x.option


and gen_packed_enum_option x =
  let open O in
  let value = some_of x.t.T.Option.code in
  Piqirun.int32_to_packed_signed_varint value


and gen_list code x =
  let open L in
  if not x.t.T.Piqi_list.protobuf_packed
  then Piqirun.gen_list gen_obj code x.obj
  else Piqirun.gen_packed_list gen_packed_obj code x.obj


and gen_alias ?wire_type code x =
  let open A in
  let wire_type = resolve_wire_type ?wire_type x.t.T.Alias.protobuf_wire_type in
  match x.obj with
    | `int x | `uint x -> gen_int code x ?wire_type
    | `float x -> gen_float code x ?wire_type
    | `alias x -> gen_alias code x ?wire_type
    | obj -> gen_obj code obj


and gen_packed_alias ?wire_type x =
  let open A in
  let wire_type = resolve_wire_type ?wire_type x.t.T.Alias.protobuf_wire_type in
  match x.obj with
    | `int x | `uint x -> gen_packed_int x ?wire_type
    | `float x -> gen_packed_float x ?wire_type
    | `alias x -> gen_packed_alias x ?wire_type
    | obj -> gen_packed_obj obj


and resolve_wire_type ?wire_type this_wire_type =
  (* wire-type defined in this alias is overridden by wire-type from the
   * upper-level definition *)
  if wire_type <> None
  then wire_type
  else this_wire_type


let _ =
  Piqobj.to_pb := gen_binobj

