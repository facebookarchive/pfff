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


type xml = Piqi_xml_type.xml
type xml_elem = Piqi_xml_type.xml_elem


let handle_unknown_field = Piqobj_of_json.handle_unknown_field

let check_duplicate = Piqobj_of_piq.check_duplicate


let parse_scalar xml_elem err_string =
  let _name, l = xml_elem in
  match l with
    | [`Data s] -> s
    | _ -> error xml_elem err_string


let parse_string_scalar xml_elem err_string =
  let _name, l = xml_elem in
  match l with
    | [] -> (* empty element content means empty string *)
        let res = "" in
        Piqloc.addrefret xml_elem res
    | [`Data s] -> s
    | _ -> error xml_elem err_string


let parse_int xml_elem = 
  let s = parse_scalar xml_elem "int constant expected" in
  try
    match s.[0] with
      | '-' -> `int (Int64.of_string s) (* negative integer *)
      | _ -> `uint (Piq_parser.parse_uint s)
  with Failure _ ->
    (* NOTE: actually, there can be two errors here: invalid integer literal
     * and integer overflow *)
    error xml_elem "invalid int constant"


let parse_float xml_elem =
  let s = parse_scalar xml_elem "float constant expected" in
  match s with
    | "NaN" -> Pervasives.nan
    | "Infinity" -> Pervasives.infinity
    | "-Infinity" -> Pervasives.neg_infinity
    | _ ->
        try float_of_string s
        with Failure _ ->
          error xml_elem "invalid float constant"


let parse_bool xml_elem =
  let err = "bool constant expected" in
  let s = parse_scalar xml_elem err in
  match s with
    | "true" -> true
    | "false" -> false
    | _ ->
        error xml_elem err


let parse_string xml_elem =
  parse_string_scalar xml_elem "string constant expected"


let parse_binary xml_elem =
  let s = parse_string_scalar xml_elem "binary constant expected" in
  try Piqi_base64.decode s
  with Invalid_argument _ ->
    error xml_elem "invalid base64-encoded string"


(* get the list of XML elements from the node *)
let get_record_elements (l: xml list) :xml_elem list =
  List.map (fun xml ->
    match xml with
      | `Elem x -> x
      | `Data s ->
          error xml "XML element is expected as a record field") l


let rec parse_obj (t: T.piqtype) (x: xml_elem) :Piqobj.obj =
  match t with
    (* built-in types *)
    | `int -> parse_int x
    | `float -> `float (parse_float x)
    | `bool -> `bool (parse_bool x)
    | `string -> `string (parse_string x)
    | `binary -> `binary (parse_binary x)
    | `any -> `any (parse_any x)
    (* custom types *)
    | `record t -> `record (parse_record t x)
    | `variant t -> `variant (parse_variant t x)
    | `enum t -> `enum (parse_enum t x)
    | `list t -> `list (parse_list t x)
    | `alias t -> `alias (parse_alias t x)


and parse_any xml_elem =
  match xml_elem with
    | _name, (`Elem ("piqi-type", [`Data "piqi-any"])) :: rem -> (* extended piqi-any format *)
        let rem = get_record_elements rem in
        (* manually parsing the piqi-any record, so that we could extract the
         * symbolic xml representation *)
        (* XXX: check correspondence between typed protobuf and typed xml? *)
        let typename_obj, rem = parse_optional_field "type" `string None rem in
        let protobuf_obj, rem = parse_optional_field "protobuf" `binary None rem in
        let xml_obj, rem = parse_optional_field "xml" `any None rem in
        (* issue warnings on unparsed fields *)
        List.iter handle_unknown_field rem;
        let typename =
          match typename_obj with
            | Some (`string x) -> Some x
            | _ -> None
        in
        let protobuf =
          match protobuf_obj with
            | Some (`binary x) -> Some x
            | _ -> None
        in
        let xml_ast =
          match xml_obj with
            | Some (`any {Any.xml_ast = xml_ast}) -> xml_ast
            | _ -> None
        in
        Any.({
          Piqobj.default_any with
          typename = typename;
          pb = protobuf;
          xml_ast = xml_ast;
        })
    | _ -> (* regular symbolic piqi-any *)
        Any.({
          Piqobj.default_any with
          xml_ast = Some xml_elem;
        })


and parse_record t xml_elem =
  debug "do_parse_record: %s\n" (some_of t.T.Record.name);
  (* get the list of XML elements from the node *)
  let _name, l = xml_elem in
  let l = get_record_elements l in

  (* NOTE: passing locating information as a separate parameter since empty
   * list is unboxed and doesn't provide correct location information *)
  let loc = xml_elem in

  let fields_spec = t.T.Record.field in
  let fields, rem =
    List.fold_left (parse_field loc) ([], l) fields_spec in
  (* issue warnings on unparsed fields *)
  List.iter handle_unknown_field rem;
  (* put required fields back at the top *)
  R.({t = t; field = List.rev fields; unparsed_piq_fields_ref = None})


and parse_field loc (accu, rem) t =
  let fields, rem =
    match t.T.Field.piqtype with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field loc t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let name = some_of t.name in (* flag name is always defined *)
  debug "do_parse_flag: %s\n" name;
  let res, rem = find_flags name l in
  match res with
    | [] -> [], rem
    | x::tail ->
        check_duplicate name tail;
        let res = F.({t = t; obj = None}) in
        [res], rem


and do_parse_field loc t l =
  let open T.Field in
  let name = C.name_of_field t in
  debug "do_parse_field: %s\n" name;
  let field_type = some_of t.piqtype in
  let values, rem =
    match t.mode with
      | `required -> 
          let x, rem = parse_required_field loc name field_type l in
          [x], rem
      | `optional ->
          let x, rem = parse_optional_field name field_type t.default l in
          let res = (match x with Some x -> [x] | None -> []) in
          res, rem
      | `repeated ->
          parse_repeated_field name field_type l
  in
  let fields =
    List.map (fun x -> F.({t = t; obj = Some x})) values
  in
  fields, rem
  

and parse_required_field loc name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> error loc ("missing field " ^ U.quote name)
    | x::tail ->
        check_duplicate name tail;
        parse_obj field_type x, rem


(* find field by name, return found fields and remaining fields *)
and find_fields (name:string) (l:xml_elem list) :(xml_elem list * xml_elem list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | ((n, _) as h)::t when n = name -> aux (h::accu) rem t
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (l:xml_elem list) :(string list * xml_elem list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, [])::t when n = name -> aux (n::accu) rem t
    | (n, _)::t when n = name ->
        error n ("value can not be specified for flag " ^ U.quote n)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and parse_optional_field name field_type default l =
  let res, rem = find_fields name l in
  match res with
    | [] -> Piqobj_common.parse_default field_type default, rem
    | x::tail ->
        check_duplicate name tail;
        Some (parse_obj field_type x), rem


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> [], rem (* allowing repeated field to be acutally missing *)
    | l ->
        let res = List.map (parse_obj field_type) l in
        res, rem


and parse_variant t xml_elem =
  debug "parse_variant: %s\n" (some_of t.T.Variant.name);
  let _name, l = xml_elem in
  match l with
    | [`Elem ((name, _) as xml_elem)] ->
        let options = t.T.Variant.option in
        let option =
          try
            let o = List.find (fun o -> name = C.name_of_option o) options in
            parse_option o xml_elem
          with Not_found ->
            error xml_elem ("unknown variant option: " ^ U.quote name)
        in
        V.({t = t; option = option})
    | _ ->
        error xml_elem "exactly one XML element expected as a variant value"


and parse_option t xml_elem =
  let open T.Option in
  let name, l = xml_elem in
  match t.piqtype, l with
    | None, [] ->
        O.({ t = t; obj = None})
    | None, _ ->
        error name ("no value expected for option flag " ^ U.quote name)
    | Some option_type, _ ->
        let obj = parse_obj option_type xml_elem in
        O.({t = t; obj = Some obj})


and parse_enum t xml_elem =
  debug "parse_enum: %s\n" (some_of t.T.Enum.name);
  let name =
    parse_scalar xml_elem "exactly one XML CDATA expected as an enum value"
  in
  let options = t.T.Enum.option in
  let option =
    try
      let o = List.find (fun o -> some_of o.T.Option.name = name) options in
      O.({t = o; obj = None})
    with Not_found ->
      error name ("unknown enum option: " ^ U.quote name)
  in
  E.({t = t; option = option})


and parse_list t xml_elem =
  debug "parse_list: %s\n" (some_of t.T.Piqi_list.name);
  let obj_type = some_of t.T.Piqi_list.piqtype in
  let _name, l = xml_elem in
  let contents = List.map (parse_list_item obj_type) l in
  L.({t = t; obj = contents})


and parse_list_item obj_type xml =
  debug "parse_list_item\n";
  match xml with
    | `Elem (("item", l) as xml_elem) ->
        parse_obj obj_type xml_elem
    | _ ->
        error xml "<item> XML element expected as a list item value"


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t x =
  let open T.Alias in
  let obj_type = some_of t.piqtype in
  debug "parse_alias: %s\n" (some_of t.T.Alias.name);
  let obj = parse_obj obj_type x in
  A.({t = t; obj = obj})


let _ =
  Piqobj.of_xml := parse_obj


(* parse top-level Piq object formatted as XML *)
let parse_obj t xml =
  (* NOTE: we don't bother checking the name of the root element -- it doesn't
   * have any meaning anyway *)
  match xml with
    | `Elem xml_elem ->
        parse_obj t xml_elem
    | _ ->
        error xml "XML root element expected"

