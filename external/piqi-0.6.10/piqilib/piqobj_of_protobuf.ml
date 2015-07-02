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


let next_count = Piqloc.next_icount


let reference0 f x =
  let count = next_count () in
  let obj = f x in
  Piqloc.addrefret count obj


let reference f t x =
  let count = next_count () in
  let obj = f t x in
  Piqloc.addrefret count obj


(* XXX: move to Piqi_protobuf? *)
let parse_int ?wire_type x =
  let r0 = reference0 in
  let wire_type = W.get_wire_type `int wire_type in
  match wire_type with
    | `varint -> `uint (r0 Piqirun.int64_of_varint x)
    | `zigzag_varint -> `int (r0 Piqirun.int64_of_zigzag_varint x)
    | `fixed32 -> `uint (r0 Piqirun.int64_of_fixed32 x)
    | `fixed64 -> `uint (r0 Piqirun.int64_of_fixed64 x)
    | `signed_varint -> `int (r0 Piqirun.int64_of_signed_varint x)
    | `signed_fixed32 -> `int (r0 Piqirun.int64_of_signed_fixed32 x)
    | `signed_fixed64 -> `int (r0 Piqirun.int64_of_signed_fixed64 x)
    | `block -> assert false (* XXX *)


let parse_packed_int ?wire_type x =
  let wire_type = W.get_wire_type `int wire_type in
  match wire_type with
    | `varint -> `uint (Piqirun.int64_of_packed_varint x)
    | `zigzag_varint -> `int (Piqirun.int64_of_packed_zigzag_varint x)
    | `fixed32 -> `uint (Piqirun.int64_of_packed_fixed32 x)
    | `fixed64 -> `uint (Piqirun.int64_of_packed_fixed64 x)
    | `signed_varint -> `int (Piqirun.int64_of_packed_signed_varint x)
    | `signed_fixed32 -> `int (Piqirun.int64_of_packed_signed_fixed32 x)
    | `signed_fixed64 -> `int (Piqirun.int64_of_packed_signed_fixed64 x)
    | `block -> assert false (* XXX *)


let parse_float ?wire_type x =
  let r0 = reference0 in
  let wire_type = W.get_wire_type `float wire_type in
  match wire_type with
    | `fixed32 -> r0 Piqirun.float_of_fixed32 x
    | `fixed64 -> r0 Piqirun.float_of_fixed64 x
    | _ -> assert false (* XXX *)


let parse_packed_float ?wire_type x =
  let wire_type = W.get_wire_type `float wire_type in
  match wire_type with
    | `fixed32 -> Piqirun.float_of_packed_fixed32 x
    | `fixed64 -> Piqirun.float_of_packed_fixed64 x
    | _ -> assert false (* XXX *)


let rec parse_obj0 (t:T.piqtype) x :Piqobj.obj =
  let r0 = reference0 in
  let r = reference in
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (parse_float x)
    | `bool -> `bool (r0 Piqirun.parse_bool_field x)
    | `string -> `string (r0 Piqirun.parse_string_field x)
    | `binary -> `binary (r0 Piqirun.parse_binary_field x)
    | `any -> `any (parse_any x)
    (* custom types *)
    | `record t -> `record (r parse_record t x)
    | `variant t -> `variant (r parse_variant t x)
    | `enum t -> `enum (r parse_enum t x)
    | `list t -> `list (r parse_list t x)
    | `alias t -> `alias (parse_alias0 t x)


and parse_packed_obj (t:T.piqtype) x :Piqobj.obj =
  match t with
    (* built-in types *)
    | `int -> parse_packed_int x
    | `float -> `float (parse_packed_float x)
    | `bool -> `bool (Piqirun.bool_of_packed_varint x)
    | `enum t -> `enum (parse_packed_enum t x)
    | `alias t -> `alias (parse_packed_alias t x)
    | _ ->
        assert false (* objects of other types can't be packed *)


and parse_obj t x =
  (* using the same count here as we will use for parsing the objects themselves
   *)
  let count = !Piqloc.icount in
  let res = parse_obj0 t x in
  Piqloc.addrefret count res


and parse_binobj piqtype binobj =
  Piqirun.parse_binobj (parse_obj piqtype) binobj


and parse_any x =
  let piqi_any = T.parse_any x in
  match piqi_any.T.Any.ref with
    | Some ref ->
        (* recover internally passed Piqobj.any from an integer reference *)
        C.debug "Piqobj_of_protobuf.parse_any: recovering any from existing ref %d\n" ref;
        Piqobj.get_any ref
    | None ->
        (* external mode *)
        let any = Any.({
          Piqobj.default_any with
          typename = piqi_any.Piqi_impl_piqi.Any.typename;
          pb = piqi_any.Piqi_impl_piqi.Any.protobuf;
        })
        in
        (* if there's no typed protobuf, look for untyped JSON or XML values *)
        if any.Any.pb = None
        then (
          let json = piqi_any.T.Any.json in
          let xml = piqi_any.T.Any.xml in
          match json, xml with
            | Some s, _ ->
                let json_ast = !Piqobj.json_of_string s in
                any.Any.json_ast <- Some json_ast; (* same as in Piqobj_of_piq.parse_any *)
                any.Any.json_string <- Some s
            | _, Some s ->
                let xml_list = !Piqobj.xml_of_string s in
                any.Any.xml_ast <- Some ("undefined", xml_list) (* same as in Piqobj_of_piq.parse_any *)
            | None, None ->
                (* XXX: hmm... no protobuf, JSON, XML -- what are we dealing with
                 * here? *)
                ()
        );
        any


and parse_record t x =
  let l = Piqirun.parse_record x in
  (* obtain the reference to unparsed piq fields if we are dealing with
   * serialized Piqi object *)
  let unparsed_piq_fields_ref, l =
    if !C.is_inside_parse_piqi
    then parse_unparsed_piq_fields_ref l
    else None, l
  in
  (* NOTE: fields are pre-order by wire code *)
  let fields_spec = t.T.Record.wire_field in
  let fields, rem = List.fold_left parse_field ([], l) fields_spec in
  Piqirun.check_unparsed_fields rem;
  R.({t = t; field = List.rev fields; unparsed_piq_fields_ref = unparsed_piq_fields_ref})


and parse_unparsed_piq_fields_ref l =
  let res = Piqirun.parse_optional_field 1 (fun x -> parse_int ~wire_type:`varint x) l in
  match res with
    | Some (`uint x), rem -> Some (Int64.to_int x), rem
    | None, l -> None, l
    | _ -> assert false


and parse_field (accu, rem) t =
  let fields, rem =
    match t.T.Field.piqtype with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let code = Int32.to_int (some_of t.code) in
  let res, rem = Piqirun.parse_flag code l in
  match res with
    | false -> [], rem
    | true ->
        begin
          let res = F.({t = t; obj = None}) in

          (* skip boolean used to encode empty flag value *)
          let count = next_count () in
          Piqloc.addref count res;

          [res], rem
        end


and do_parse_field t l =
  let open T.Field in
  let code = Int32.to_int (some_of t.code) in
  let field_type = some_of t.piqtype in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field code field_type l in
          [x], rem
      | `optional ->
          (* XXX: location reference for default? *)
          let x, rem = parse_optional_field code field_type t.default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          if not t.protobuf_packed
          then parse_repeated_field code field_type l
          else parse_packed_repeated_field code field_type l
  in
  let fields =
    List.map (fun x ->
      let res = F.({t = t; obj = Some x}) in
      Piqloc.addrefret x res) values
  in
  fields, rem


and parse_required_field code field_type l =
  Piqirun.parse_required_field code (parse_obj field_type) l


and parse_optional_field code field_type default l =
  let res = Piqirun.parse_optional_field code (parse_obj field_type) l in
  match res with
    | Some _, _ -> res
    | None, rem ->
        Piqobj_common.parse_default field_type default, rem


and parse_repeated_field code field_type l =
  Piqirun.parse_repeated_field code (parse_obj field_type) l


and parse_packed_repeated_field code field_type l =
  Piqirun.parse_packed_repeated_field code
    (parse_packed_obj field_type) (parse_obj field_type) l


and parse_variant t x =
  let code, obj = Piqirun.parse_variant x in
  let code32 = Int32.of_int code in
  let options = t.T.Variant.option in
  let option =
    try
      let o = List.find (fun o -> some_of o.T.Option.code = code32) options in
      parse_option o obj
    with Not_found ->
      Piqirun.error_variant x code
  in
  V.({t = t; option = option})


and parse_option t x =
  let open T.Option in
  match t.piqtype with
    | None ->
        if Piqirun.parse_bool_field x = true
        then
          let res = O.({t = t; obj = None}) in
          (* skip boolean used to encode empty option value *)
          let count = next_count () in
          Piqloc.addrefret count res
        else
          piqi_error "invalid representation of untyped option"
    | Some option_type ->
        let obj = parse_obj option_type x in
        let res = O.({t = t; obj = Some obj}) in
        Piqloc.addrefret obj res


and parse_enum t x =
  let code32 = Piqirun.int32_of_signed_varint x in
  let option =
    try parse_enum_option t code32
    with Not_found ->
      Piqirun.error_enum_const x
  in
  (* add location reference which is equal to the enum location *)
  Piqloc.addref !Piqloc.icount option;
  E.({t = t; option = option})


and parse_packed_enum t x =
  let code32 = Piqirun.int32_of_packed_signed_varint x in
  let option =
    try parse_enum_option t code32
    with Not_found ->
      Piqirun.error_enum_const x
  in
  E.({t = t; option = option})


and parse_enum_option t code32 =
  let options = t.T.Enum.option in
  let o = List.find (fun o -> some_of o.T.Option.code = code32) options in
  O.({t = o; obj = None})


and parse_list t x = 
  let obj_type = some_of t.T.Piqi_list.piqtype in
  let contents =
    if not t.T.Piqi_list.protobuf_packed
    then Piqirun.parse_list (parse_obj obj_type) x
    else Piqirun.parse_packed_list
      (parse_packed_obj obj_type) (parse_obj obj_type) x
  in
  L.({t = t; obj = contents})


and parse_alias0 t x =
  parse_alias t x


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t ?wire_type x =
  let open T.Alias in
  let wire_type = Piqobj_to_protobuf.resolve_wire_type ?wire_type t.protobuf_wire_type in
  let obj =
    match some_of t.piqtype with
      | `int -> parse_int x ?wire_type
      | `float -> `float (parse_float x ?wire_type)
      | `alias t -> `alias (parse_alias t x ?wire_type)
      | t -> parse_obj t x
  in
  A.({t = t; obj = obj})


and parse_packed_alias t ?wire_type x =
  let open T.Alias in
  let wire_type = Piqobj_to_protobuf.resolve_wire_type ?wire_type t.protobuf_wire_type in
  let obj =
    match some_of t.piqtype with
      | `int -> parse_packed_int x ?wire_type
      | `float -> `float (parse_packed_float x ?wire_type)
      | `alias t -> `alias (parse_packed_alias t x ?wire_type)
      | t -> parse_packed_obj t x
  in
  A.({t = t; obj = obj})


let _ =
  Piqobj.of_pb := parse_binobj

