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


type json = Piqi_json_type.json


let error_duplicate obj name =
  error obj ("duplicate field: " ^ U.quote name)


let handle_unknown_field ((n, _) as x) =
  let f =
    if !Config.flag_strict
    then error
    else warning
  in
  f x ("unknown field: " ^ U.quote n)


let parse_int (obj:json) = match obj with
  | `Int x -> `int x
  | `Uint x -> `uint x
  | o -> error o "int constant expected"


let parse_float (x:json) = match x with
  | `Int x -> Int64.to_float x
  | `Uint x -> Piqobj_of_piq.uint64_to_float x
  | `Float x -> x
  | `String "NaN" -> Pervasives.nan
  | `String "Infinity" -> Pervasives.infinity
  | `String "-Infinity" -> Pervasives.neg_infinity
  | o -> error o "float constant expected"


let parse_bool (x:json) = match x with
  | `Bool x -> x
  | o -> error o "bool constant expected"


let parse_string (x:json) = match x with
  | `String x -> x
  | o -> error o "string constant expected"


let parse_binary (x:json) = match x with
  | `String x ->
      (try Piqi_base64.decode x
      with Invalid_argument _ ->
        error x "invalid base64-encoded string"
      )
  | o -> error o "string constant expected"


let rec parse_obj (t:T.piqtype) (x:json) :Piqobj.obj =
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


and parse_any (x:json) :Piqobj.any =
  (* detect extended piqi-any format *)
  match x with
    | `Assoc (("piqi_type", `String "piqi-any") :: rem) -> (* extended piqi-any format *)
        (* manually parsing the piqi-any record, so that we could extract the
         * symbolic json representation *)
        (* XXX: check correspondence between typed protobuf and typed json? *)
        let typename_obj, rem = parse_optional_field "type" `string None rem in
        let protobuf_obj, rem = parse_optional_field "protobuf" `binary None rem in
        let json_obj, rem = parse_optional_field "json" `any None rem in
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
        let json_ast =
          match json_obj with
            | Some (`any {Any.json_ast = json_ast}) -> json_ast
            | _ -> None
        in
        Any.({
          Piqobj.default_any with
          typename = typename;
          pb = protobuf;
          json_ast = json_ast;
        })
    | json_ast -> (* regular symbolic piqi-any *)
        (* TODO: preserve the original int, float and string literals -- see
         * Piqobj.json_of_any *)
        Any.({
          Piqobj.default_any with
          json_ast = Some json_ast;
        })


and parse_record t = function
  | (`Assoc l) as x ->
      (* NOTE: passing locating information as a separate parameter since empty
       * list is unboxed and doesn't provide correct location information *)
      let loc = x in
      do_parse_record loc t l
  | o ->
      error o "object expected"


and do_parse_record loc t l =
  debug "do_parse_record: %s\n" (some_of t.T.Record.name);
  let fields_spec = t.T.Record.field in
  let fields, rem =
    List.fold_left (parse_field loc) ([], l) fields_spec in
  (* issue warnings on unparsed fields *)
  List.iter handle_unknown_field rem;
  (* put required fields back at the top *)
  R.({ t = t; field = List.rev fields; unparsed_piq_fields_ref = None})


and parse_field loc (accu, rem) t =
  let fields, rem =
    match t.T.Field.piqtype with
      | None -> do_parse_flag t rem
      | Some _ -> do_parse_field loc t rem
  in
  (List.rev_append fields accu, rem)


and do_parse_flag t l =
  let open T.Field in
  let name = some_of t.json_name in
  debug "do_parse_flag: %s\n" name;
  let res, rem = find_flags name l in
  match res with
    | [] -> [], rem
    | [x] ->
        let res = F.({t = t; obj = None}) in
        [res], rem
    | _::o::_ -> error_duplicate o name


and do_parse_field loc t l =
  let open T.Field in
  let name = some_of t.json_name in
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
    | [x] -> parse_obj field_type x, rem
    | _::o::_ -> error_duplicate o name


(* find field by name, return found fields and remaining fields *)
and find_fields (name:string) (l:(string*json) list) :(json list * (string*json) list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, v)::t when n = name -> aux (v::accu) rem t
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


(* find flags by name, return found flags and remaining fields *)
and find_flags (name:string) (l:(string*json) list) :(string list * (string*json) list) =
  let rec aux accu rem = function
    | [] -> List.rev accu, List.rev rem
    | (n, `Bool true)::t when n = name -> aux (n::accu) rem t
    | (n, `Null ())::t when n = name -> aux accu rem t (* skipping *)
    | (n, _)::t when n = name ->
        error n ("value can not be specified for flag " ^ U.quote n)
    | h::t -> aux accu (h::rem) t
  in
  aux [] [] l


and parse_optional_field name field_type default l =
  let res, rem = find_fields name l in
  match res with
    | [] -> Piqobj_common.parse_default field_type default, rem
    | [`Null ()] -> None, rem
    | [x] -> Some (parse_obj field_type x), rem
    | _::o::_ -> error_duplicate o name


(* parse repeated variant field allowing variant names if field name is
 * unspecified *) 
and parse_repeated_field name field_type l =
  let res, rem = find_fields name l in
  match res with
    | [] -> [], rem (* XXX: allowing repeated field to be acutally missing *)
    | [`List l] ->
        let res = List.map (parse_obj field_type) l in
        res, rem
    | [x] -> error x "array expected"
    | _::o::_ -> error_duplicate o name


and parse_variant t x =
  debug "parse_variant: %s\n" (some_of t.T.Variant.name);
  match x with
    | `Assoc [name, value] ->
        let options = t.T.Variant.option in
        let option =
          try
            let o =
              List.find (fun o ->
                some_of o.T.Option.json_name = name) options
            in
            parse_option o value
          with Not_found ->
            error x ("unknown variant option: " ^ U.quote name)
        in
        V.({t = t; option = option})
    | `Assoc l ->
        let l = List.filter (fun (n, v) -> v <> `Null ()) l in
        (match l with
          | [_] -> parse_variant t (`Assoc l)
          | _ -> error x "exactly one non-null option field expected"
        )
    | _ ->
        error x "object expected"


and parse_option t x =
  let open T.Option in
  match t.piqtype, x with
    | None, `Bool true ->
        O.({t = t; obj = None})
    | None, _ ->
        error x "true value expected"
    | Some option_type, _ ->
        let obj = parse_obj option_type x in
        O.({t = t; obj = Some obj})


and parse_enum t x =
  debug "parse_enum: %s\n" (some_of t.T.Enum.name);
  match x with
    | `String name ->
        let options = t.T.Enum.option in
        let option =
          try
            let o = List.find (fun o -> some_of o.T.Option.json_name = name) options in
            O.({t = o; obj = None})
          with Not_found ->
            error x ("unknown enum option: " ^ U.quote name)
        in
        E.({t = t; option = option})
    | _ ->
        error x "string enum value expected"


and parse_list t x =
  match x with
    | `List l ->
        debug "parse_list: %s\n" (some_of t.T.Piqi_list.name);
        let obj_type = some_of t.T.Piqi_list.piqtype in
        let contents = List.map (parse_obj obj_type) l in
        L.({t = t; obj = contents})
    | _ ->
        error x "array expected"


(* XXX: roll-up multiple enclosed aliases into one? *)
and parse_alias t x =
  let open T.Alias in
  let obj_type = some_of t.piqtype in
  debug "parse_alias: %s\n" (some_of t.T.Alias.name);
  let obj = parse_obj obj_type x in
  A.({t = t; obj = obj})


let _ =
  Piqobj.of_json := parse_obj

