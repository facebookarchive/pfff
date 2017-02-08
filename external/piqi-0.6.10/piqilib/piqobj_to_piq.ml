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


(* whether to generate piqi-any as :typed or leave it as piqi-any (this depends
 * on whether we are going to pretty-print the ast or not)
 *)
let is_external_mode = ref false


(* NOTE, XXX: losing precision here, in future we will support encoding floats
 * as string literals containing binary representation of 64-bit IEEE float *)
let gen_float x = `float (x, "")


let is_ascii_string s =
  let len = String.length s in
  let rec aux i =
    if i >= len
    then true
    else
      if Char.code s.[i] <= 127
      then aux (i + 1)
      else false
  in
  aux 0


let gen_string ?piq_format s =
  match piq_format with
   | Some `text ->
       (* TODO: check if we can actually represent it as verbatim text; make
        * sure there are no non-printable characters *)
       `text s
   | Some `word when Piq_lexer.is_valid_word s ->
       `word s
   | _ ->
      if is_ascii_string s
      then
        `ascii_string (s, "")
      else
        `utf8_string (s, "")


let gen_binary s =
  if is_ascii_string s
  then
    `ascii_string (s, "")
  else
    `binary (s, "")


let make_named name value =
  `named Piq_ast.Named.({name = name; value = value})


let make_name name =
  `name name


let make_typed typename ast :piq_ast =
  let res = Piq_ast.Typed.({typename = typename; value = ast}) in
  Piqloc.addref ast res;
  `typed res


(* (re-)order fields according to their positions in the original piqi spec *)
let order_record_fields t piqobj_fields =
  let find_fields ft l =
    List.partition (fun x -> x.F.t == ft) l
  in
  let res, _rem =
    List.fold_left
      (fun (accu, rem) x -> (* folder *)
        let res, rem' = find_fields x rem in
        (List.rev_append res accu, rem'))

      ([], piqobj_fields) (* accu *)

      t.T.Record.field (* list to fold *)
  in
  List.rev res


let rec gen_obj0 ?(piq_format: T.piq_format option) (x:Piqobj.obj) :piq_ast =
  match x with
    (* built-in types *)
    | `int x -> `int (x, "")
    | `uint x -> `uint (x, "")
    | `float x -> gen_float x
    | `bool x -> `bool x
    | `string x -> gen_string x ?piq_format
    | `binary x -> gen_binary x
    | `any x -> gen_any x
    (* custom types *)
    | `record x -> gen_record x
    | `variant x -> gen_variant x
    | `enum x -> gen_enum x
    | `list x -> gen_list x
    | `alias x -> gen_alias x ?piq_format


(* TODO: provide more precise locations for fields, options, etc *)
and gen_obj ?piq_format x =
  let res = gen_obj0 x ?piq_format in
  match res with
    | `any any ->
        Piqloc.addrefret x res
    | _ ->
        Piq_parser.piq_addrefret x res


and gen_typed_obj x =
  let name = Piqobj_common.full_typename x in
  `typed Piq_ast.Typed.({typename = name; value = gen_obj x})


and gen_any x =
  let open Any in
  if not !is_external_mode
  then
    (* in internal mode, passing a reference to intermediate Any prepresentation
     * registered using Piqi_objstore *)
    `any (Piqobj.put_any x)
  else (
    let ast = Piqobj.piq_of_any x in
    match x.typename, ast with
      | Some typename, Some ast ->
          make_typed typename ast
      | None, Some ast ->
          ast
      | Some _, None ->
          assert false (* this is an impossible case *)
      | None, None -> (
          (* support for untyped JSON and XML *)
          match Piqobj.json_of_any x, Piqobj.xml_of_any x with
            | None, None ->
                (* sometimes this can happen in external mode, for example, when
                 * doing piq -> portable format -> piq conversion of untyped
                 * piqi-any values; there's not much we can do here, because we
                 * don't represent non-portable Piq ast in a portable
                 * serialization format *)
                `any 0
            | Some json_ast, _ ->
                let s = !Piqobj.string_of_json json_ast in
                `form (`word "json", [`text s]) (* (json ...) form *)
            | None, Some xml_elems ->
                let s = !Piqobj.string_of_xml (`Elem ("value", xml_elems)) in
                `form (`word "xml", [`text s]) (* (xml ...) form *)
      )
  )


and gen_record x =
  let open R in
  (* TODO, XXX: doing ordering at every generation step is inefficient *)
  let fields = order_record_fields x.t x.field in
  let encoded_fields =  List.map gen_field fields in
  let encoded_fields =
    match x.unparsed_piq_fields_ref with
      | None -> encoded_fields
      | Some ref ->
          let unparsed_fields = Piqi_objstore.get ref in
          encoded_fields @ unparsed_fields
  in
  `list encoded_fields


and gen_field x =
  let open F in
  let name = name_of_field x.t in
  let res =
    match x.obj with
      | None ->
          make_name name
      | Some obj ->
          make_named name (gen_obj obj ?piq_format:x.t.T.Field.piq_format)
  in
  Piq_parser.piq_addrefret x res


and gen_variant x =
  let open V in
  gen_option x.option


and gen_option x =
  let open O in
  let name = name_of_option x.t in
  let res =
    match x.obj with
      | None -> make_name name
      | Some obj -> make_named name (gen_obj obj ?piq_format:x.t.T.Option.piq_format)
  in Piq_parser.piq_addrefret x res


and gen_enum x =
  let open E in
  gen_option x.option


and gen_list x = 
  let open L in
  `list (List.map (fun obj -> gen_obj obj ?piq_format:x.t.T.Piqi_list.piq_format) x.obj)


and gen_alias ?(piq_format: T.piq_format option) x =
  let open A in
  (* upper-level setting overrides lower-level setting *)
  let this_piq_format = x.t.T.Alias.piq_format in
  let piq_format =
    if this_piq_format <> None
    then this_piq_format
    else piq_format
  in
  match x.obj with
    | `alias x ->
        gen_alias x ?piq_format
    | x ->
        gen_obj x ?piq_format


let gen_obj obj = gen_obj obj


let _ =
  Piqobj.to_piq := gen_obj

