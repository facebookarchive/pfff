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


(* configuration/command-line option:
 *
 * omit missing fields in JSON output; if false, they will be present in the
 * output with their values set to "null" for optional fields and "[]" for
 * repeated fields
 *)
let omit_missing_fields = ref true


let omit_missing f =
  (* use the per-field schema-level setting when specified, default to the
   * run-time setting otherwise *)
  match f.T.Field.json_omit_missing with
    | Some x -> x
    | None -> !omit_missing_fields


let make_named name value =
  name, value


let make_name name =
  name, `Bool true


let rec gen_obj (x:Piqobj.obj) :json =
  match x with
    (* built-in types *)
    | `int x -> `Int x
    | `uint x -> `Uint x
    | `float x -> `Float x
    | `bool x -> `Bool x
    | `string x -> `String x
    | `binary x -> `String (Piqi_base64.encode x)
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x


and gen_any x =
  let open Any in
  if not !Piqi_config.gen_extended_piqi_any
  then
    match Piqobj.json_of_any x with
      | None -> `Null ()
      | Some json_ast -> json_ast
  else (* non-sybolic piqi-any representation *)
    let make_json_field name value f =
      match value with
        | None when !omit_missing_fields -> []
        | None ->
            [name, `Null ()]
        | Some x ->
            [name, f x]
    in
    let typename = make_json_field "type" x.typename (fun name -> `String name)
    in
    let protobuf = make_json_field "protobuf" (Piqobj.pb_of_any x) (fun pb ->
      `String (Piqi_base64.encode pb))
    in
    let json = make_json_field "json" (Piqobj.json_of_any x) (fun json_ast ->
      json_ast)
    in
    `Assoc (
      (* this field indicates that this is an extended piqi-any representation
       * (it is necessary for detecting which variant of piqi-any represenation
       * is used and to make either representation automatically reversible) *)
      ("piqi_type", `String "piqi-any") ::
      (* actual content *)
      (typename @ protobuf @ json)
    )


and gen_record x =
  let open R in
  let field_types = x.t.T.Record.field in
  `Assoc (U.flatmap (gen_field x.field) field_types)


and gen_field fields t =
  let open T.Field in
  let name = some_of t.json_name in
  let open F in
  let pred f = f.t == t in
  match t.mode with
    | `required | `optional ->
        (try
          let f = List.find pred fields in
          let res =
            match f.obj with
               | None -> make_name name (* flag *)
               | Some obj -> make_named name (gen_obj obj)
          in [res]
        with
          Not_found ->
            if omit_missing t
            then []
            else [make_named name (`Null ())]
        )
    | `repeated ->
        let fields = List.find_all pred fields in
        if fields = [] && omit_missing t
        then []
        else
          let json_fields = List.map (fun f -> gen_obj (some_of f.obj)) fields in
          let res = make_named name (`List json_fields) in
          [res]


and gen_variant x =
  let open V in
  let o = gen_option x.option in
  `Assoc [o]


and gen_option x =
  let open O in
  let name = some_of x.t.T.Option.json_name in
  match x.obj with
    | None -> make_name name
    | Some obj -> make_named name (gen_obj obj)


and gen_enum x =
  let open E in
  gen_enum_option x.option


and gen_enum_option x =
  let open O in
  let name = some_of x.t.T.Option.json_name in
  `String name


and gen_list x = 
  let open L in
  `List (List.map gen_obj x.obj)


and gen_alias x =
  let open A in
  match x.obj with
    | `alias x -> gen_alias x
    | x -> gen_obj x


let _ =
  Piqobj.to_json := gen_obj

