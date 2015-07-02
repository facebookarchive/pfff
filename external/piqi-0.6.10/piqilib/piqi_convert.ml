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

(*
 * Protobuf - JSON - XML - Piq data conversion
 *)

module C = Piqi_common  
open C


exception EOF


(* internal representation of an object being convert *)
type obj =
  | Piqtype of string
  | Typed_piqobj of Piqobj.obj
  | Piqobj of Piqobj.obj
  | Piqi of T.piqi


(* the list of piqi objects loaded but not processed yet *)
let unprocessed_piqi_list = ref []


let process_unprocessed_piqi () =
  List.iter (fun modname ->
    let piqi = Piqi_db.find_piqi modname in
    (* process piqi if it hasn't been fully processed yet by this point *)
    ignore (Piqi.process_piqi piqi ~cache:false);  (* already cached *)
    Piqloc.preserve ()
  )
  (List.rev !unprocessed_piqi_list);
  (* reset the unprocessed piqi list *)
  unprocessed_piqi_list := []


let pre_process_piqi ~fname ?ast piqi =
  piqi.P.is_embedded <- Some true;

  (* can't process it right away, because not all dependencies could be loaded
   * already; this is expecially ciritical in case of mutually-recursive
   * includes; just do bare minimum so that we could add to Piqi_db and process
   * it later *)
  let piqi = Piqi.pre_process_piqi piqi ~fname ?ast in

  (* so that we could find it by name later *)
  Piqi_db.add_piqi piqi;

  (* preserve location information so that exising location info for Piqi
   * modules won't be discarded by subsequent Piqloc.reset() calls *)
  Piqloc.preserve ();

  (* add to the list of unprocessed modules *)
  let modname = some_of piqi.P.modname in
  unprocessed_piqi_list := modname :: !unprocessed_piqi_list;

  piqi


let default_piqtype = ref None


let check_piqtype n =
  if not (Piqi_name.is_valid_typename n)
  then error n ("invalid type name: " ^ U.quote n)
  else ()


let find_piqtype ?(check=false) typename =
  if check
  then check_piqtype typename;

  (* in case typename is defined in one of the yet unprocessed piqi modules *)
  process_unprocessed_piqi ();

  try Piqi_db.find_piqtype typename
  with Not_found ->
    error typename ("unknown type: " ^ typename)


let process_default_piqtype ?check typename =
  let piqtype = find_piqtype ?check typename in
  (* NOTE: silently overriding previous value *)
  default_piqtype := Some piqtype


(* default piqtype taken from the stream overrides the user-specified
 * one *)
let get_current_piqtype user_piqtype locref =
  match !default_piqtype, user_piqtype with
  | Some x, _ -> x
  | None, Some x -> x
  | None, None ->
      error locref "type of object is unknown"


let read_piq_ast piq_parser user_piqtype :piq_ast = if not !Piqi_config.piq_frameless_input  (* regular (framed) mode *) then
    match Piq_parser.read_next piq_parser with
      | Some ast -> ast
      | None -> raise EOF
  else  (* frameless mode *)
    let is_scalar_type ast =
      match ast with
        | `typename _ | `typed _ ->
            true
        | _ ->
            let piqtype = get_current_piqtype user_piqtype ast in
            not (C.is_container_type piqtype)
    in
    let ast_list = Piq_parser.read_all piq_parser in
    (* if there's more that one element or we're parsing value for a container
     * type, wrap elements into a list *)
    match ast_list with
      | [ast] when is_scalar_type ast ->
          ast
      | [] ->
          let ast = `list [] in
          let fname, _ = piq_parser in
          let loc = (fname, 0, 0) in
          Piqloc.addlocret loc ast
      | h::_ ->
          (* setting list location based on the location of the first element *)
          let ast = `list ast_list in
          Piqloc.addrefret h ast


let piqi_of_piq fname ast =
  let piqi = Piqi.parse_piqi ast in
  pre_process_piqi piqi ~fname ~ast


let load_piq (user_piqtype: T.piqtype option) piq_parser :obj =
  let ast = read_piq_ast piq_parser user_piqtype in
  let fname, _ = piq_parser in (* TODO: improve getting a filename from parser *)
  match ast with
    | `typename typename ->
        (* (:typename) *)
        process_default_piqtype typename;
        Piqtype typename
    | `typed {Piq_ast.Typed.typename = "piqi";
              Piq_ast.Typed.value = ((`list _) as ast)} ->
        (* :piqi <piqi-lang> *)
        let piqi = piqi_of_piq fname ast in
        Piqi piqi
    | `typed {Piq_ast.Typed.typename = "piqi"} ->
        error ast "invalid piqi specification"
    | `typed _ ->
        (* :typename ... *)
        (* in case typename is defined in one of the yet unprocessed piqi
         * modules: *)
        process_unprocessed_piqi ();
        let obj = Piqobj_of_piq.parse_typed_obj ast in
        Typed_piqobj obj
    | _ ->
        let piqtype = get_current_piqtype user_piqtype ast in
        if piqtype == !Piqi.piqi_lang_def (* XXX *)
        then
          let piqi = piqi_of_piq fname ast in
          Piqi piqi
        else
          let obj = Piqobj_of_piq.parse_obj piqtype ast in
          Piqobj obj


let original_piqi piqi =
  let orig_piqi = some_of piqi.P.original_piqi in
  (* make sure that the module's name is set *)
  P.({orig_piqi with modname = piqi.P.modname})


let piqi_ast_to_piq piqi_ast =
  let piqi_ast = Piqi_pp.prettify_piqi_ast piqi_ast in
  `typed {
    Piq_ast.Typed.typename = "piqi";
    Piq_ast.Typed.value = piqi_ast;
  }


let piqi_to_piq piqi =
  let piqi_ast = Piqi.piqi_to_ast (original_piqi piqi) in
  piqi_ast_to_piq piqi_ast


let gen_piq (obj :obj) =
  Piqloc.pause (); (* no need to preserve location information here *)
  let f () =
    match obj with
      | Piqtype typename ->
          `typename typename
      | Piqi piqi ->
          piqi_to_piq piqi
      | Typed_piqobj obj ->
          Piqobj_to_piq.gen_typed_obj obj
      | Piqobj obj ->
          Piqobj_to_piq.gen_obj obj
  in
  let res = U.with_bool Piqobj_to_piq.is_external_mode true f in
  Piqloc.resume ();
  res


(* handling of piq_frameless_output *)
let write_piq_ast writer ast =
  match ast with
    | `typed {Piq_ast.Typed.value = `list l}
    | `list l when !Piqi_config.piq_frameless_output ->
        List.iter writer l
    | _ ->
        writer ast


let to_piq_channel ch (obj:obj) =
  let ast = gen_piq obj in
  write_piq_ast (Piq_gen.to_channel ch) ast;
  Pervasives.output_char ch '\n'


let to_piq_string obj =
  let ast = gen_piq obj in
  let buf = Buffer.create 256 in
  write_piq_ast (Piq_gen.to_buffer buf) ast;
  Buffer.contents buf


let read_pib_field buf =
  (* TODO: handle runtime pib read errors *)
  match Piqirun.parse_field buf with
    | Some x -> x
    | None -> raise EOF


let piqtypes = ref []

let add_piqtype code piqtype =
  if code = 1 (* default piqtype *)
  then
    (* NOTE: silently overriding previous value *)
    default_piqtype := Some piqtype
  else
    piqtypes := (code, piqtype) :: !piqtypes


let find_piqtype_by_code code =
  try
    let (_,piqtype) =
      List.find
        (function (code',_) when code = code' -> true | _ -> false)
        !piqtypes
    in piqtype
  with
    Not_found ->
      (* TODO: add stream position info *)
      piqi_error
        ("invalid field code when reading .pib: " ^ string_of_int code)


let piqobj_of_protobuf piqtype buf =
  (* don't store location references as we're loading from the binary object *)
  Piqloc.pause ();
  let obj = Piqobj_of_protobuf.parse_obj piqtype buf in
  Piqloc.resume ();
  obj


let piqobj_to_protobuf code piqobj =
  (* don't produce location references as don't care about it in general when
   * generating data *)
  Piqloc.pause ();
  (* force external mode during the conversion so that all piqi-any values are
   * generated in external format *)
  let res =
    U.with_bool Piqobj_to_protobuf.is_external_mode true
    (fun () -> Piqobj_to_protobuf.gen_obj code piqobj)
  in
  Piqloc.resume ();
  res


let process_pib_piqtype code typename =
  let piqtype =
    if typename = "piqi"
    then
      !Piqi.piqi_lang_def (* return Piqi type from embedded self-definition *)
    else
      find_piqtype typename
  in
  add_piqtype code piqtype


let piqi_of_pib protobuf =
  let piqi = Piqi.piqi_of_pb protobuf ~process:false in
  pre_process_piqi piqi ~fname:"input" (* TODO, XXX: get the actual file name *)


(* using max Protobuf wire code value for pib-typehint
 *
 * XXX: alternatively, we could use 0 or another value outside of the valid
 * code range *)
let pib_typehint_code = (1 lsl 29) - 1


let rec load_pib (user_piqtype :T.piqtype option) buf :obj =
  let field_code, field_obj = read_pib_field buf in
  if field_code = pib_typehint_code (* is this a typehint entry? *)
  then ( (* parse and process pib_typehint entry *)
    let open T.Pib_typehint in (
    Piqloc.pause ();
    let typehint = T.parse_pib_typehint field_obj in
    Piqloc.resume ();
    if typehint.piqi_type = "piqi-type" (* is this a valid piq typehint? *)
    then (
      process_pib_piqtype typehint.code typehint.typename;
      if typehint.typename = "piqi"
      then
        load_pib user_piqtype buf
      else
        Piqtype typehint.typename
    )
    else
      (* skipping invalid typehint entry; XXX: generate a warning? *)
      load_pib user_piqtype buf
  ))
  else ( (* process a regular data entry *)
    let piqtype =
      if field_code = 1
      then
        (* process a regular data entry for which a user-supplied type can be
         * applied *)
        try get_current_piqtype user_piqtype `fake
        with _ ->
          (* TODO: add stream position info *)
          piqi_error "default type for pib object is unknown"
      else
        (* process a regular explicitly typed data entry *)
        find_piqtype_by_code field_code
    in
    if piqtype == !Piqi.piqi_lang_def (* embedded Piqi spec *)
    then
      let piqi = piqi_of_pib field_obj in
      Piqi piqi
    else
      let obj = piqobj_of_protobuf piqtype field_obj in
      if field_code = 1
      then Piqobj obj
      else Typed_piqobj obj
  )


let out_piqtypes = ref []
let next_out_code = ref 2


let gen_pib_typehint code typename =
  let x = T.Pib_typehint.({
    piqi_type = "piqi-type";
    typename = typename;
    code = code;
  }) in
  Piqloc.pause ();
  let res = T.gen__pib_typehint pib_typehint_code x in
  Piqloc.resume ();
  res


let find_add_pib_typehint name =
  try
    let (_, code) =
      List.find
        (function (name',_) when name = name' -> true | _ -> false)
        !out_piqtypes
    in None, code
  with Not_found ->
    let code = !next_out_code in
    incr next_out_code;
    out_piqtypes := (name, code)::!out_piqtypes;
    let typehint = gen_pib_typehint code name in
    Some typehint, code


let gen_pib (obj :obj) =
  let pib_typehint, data =
    match obj with
      | Piqi piqi ->
          let pib_typehint, code = find_add_pib_typehint "piqi" in
          let data = Piqi.piqi_to_pb piqi ~code in
          pib_typehint, data
      | Piqtype typename ->
          let data = gen_pib_typehint 1 typename in
          None, data
      | Piqobj obj ->
          let data = piqobj_to_protobuf 1 obj in
          None, data
      | Typed_piqobj obj ->
          let typename = Piqobj_common.full_typename obj in
          let pib_typehint, code = find_add_pib_typehint typename in
          let data = piqobj_to_protobuf code obj in
          pib_typehint, data
  in
  match pib_typehint with
    | None -> data
    | Some x ->
        (* add the pib_typehint entry before the data *)
        Piqirun.OBuf.iol [ x; data]


let to_pib_channel ch (obj :obj) =
  let data = gen_pib obj in
  Piqirun.to_channel ch data


let to_pib_string obj =
  let buf = gen_pib obj in
  Piqirun.to_string buf


let load_pb (piqtype:T.piqtype) protobuf :obj =
  (* TODO: handle runtime protobuf read errors *)
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = Piqi.piqi_of_pb protobuf in
    Piqi piqi
  else
    let obj = piqobj_of_protobuf piqtype protobuf in
    Typed_piqobj obj


let gen_pb (obj :obj) =
  match obj with
    | Piqi piqi ->
        Piqi.piqi_to_pb piqi
    | Typed_piqobj obj | Piqobj obj ->
        (* -1 is a special code meaning that key and length for blocks should
         * not be generated. The resulting code is the same as generated by
         * Piqi_to_wire.gen_binobj, but this way it is returned as an output
         * buffer instead of a string in order to avoid extra memory copying *)
        piqobj_to_protobuf (-1) obj
    | Piqtype _ ->
        (* ignore default type names *)
        Piqirun.OBuf.iol [] (* == empty output *)


let to_pb_channel ch (obj :obj) =
  let buf = gen_pb obj in
  Piqirun.to_channel ch buf


let to_pb_string obj =
  let buf = gen_pb obj in
  Piqirun.to_string buf


(*
 * JSON reading and writing
 *)

let piqobj_of_json piqtype json :Piqobj.obj =
  Piqobj_of_json.parse_obj piqtype json


let piqi_of_json ?(process=true) json =
  let piqtype = !Piqi.piqi_spec_def in
  (* don't resolve defaults when reading Json *)
  let piqobj =
    C.with_resolve_defaults false (fun () -> Piqobj_of_json.parse_obj piqtype json)
  in
  (* don't try to track location references as we don't preserve them yet in
   * piqobj_of_json *)
  Piqloc.pause ();
  let piqi = Piqi.piqi_of_piqobj piqobj ~process in
  let piqi =
    if process
    then piqi
    else pre_process_piqi piqi ~fname:"input" (* TODO, XXX: get the actual file name *)
  in
  Piqloc.resume ();
  piqi


let piqi_to_json piqi =
  let piqobj = Piqi.piqi_to_piqobj piqi in
  let json = Piqobj_to_json.gen_obj piqobj in
  "piqi", json


let gen_json_obj ~plain (piqobj : Piqobj.obj) =
  let json = Piqobj_to_json.gen_obj piqobj in
  let piqtype = Piqobj_common.type_of piqobj in
  let piqtype_name = C.full_piqi_typename piqtype in
  (* generating an associative array wrapper for primitive types because JSON
   * doesn't support them as top-level objects, according to RFC 4627 that says:
   * "A JSON text is a serialized object or array" *)

  (* optionally, wrapping arrays in a top-level object; it is the only
   * reasonable way we can add "piqi_type" field to the serialized lists -- see
   * below *)
  let json =
    match json with
      | `Assoc _ -> json
      | `List _ when plain -> json
      | _ -> `Assoc [("value", json)]
  in
  piqtype_name, json


let gen_json_common ~plain (obj :obj) =
  match obj with
    | Typed_piqobj obj | Piqobj obj ->
        gen_json_obj obj ~plain
    | Piqi piqi ->
        (* output Piqi spec itself if we are converting .piqi *)
        piqi_to_json piqi
    | Piqtype _ ->
        assert false (* type hints are not supported by Json encoding *)


let gen_json obj =
  let piqi_typename, json = gen_json_common obj ~plain:false in
  (* adding "piqi_type": name as a first field of the serialized JSON object *)
  match json with
    | `Assoc l ->
        let piqi_type = ("piqi_type", `String piqi_typename) in
        `Assoc (piqi_type :: l)
    | _ -> (* top-level json must be an object *)
        assert false


let gen_plain_json obj =
  let _piqi_type, json = gen_json_common obj ~plain:true in
  json


let to_json_channel ch (obj:obj) =
  let json = gen_json obj in
  Piqi_json_gen.pretty_to_channel ch json;
  Pervasives.output_char ch '\n'


let to_json_string ?(pretty_print=true) obj =
  let json = gen_plain_json obj in
  if pretty_print
  then
    Piqi_json_gen.pretty_to_string json
  else
    Piqi_json_gen.to_string json


let read_json_ast json_parser :Piqi_json_type.json =
  let res = Piqi_json.read_json_obj json_parser in
  match res with
    | Some ast -> ast
    | None -> raise EOF


let is_primitive piqtype =
  match C.unalias piqtype with
    | `enum _ -> true
    | #T.typedef -> false
    | _ -> true


let is_list piqtype =
  match C.unalias piqtype with
    | `list _ -> true
    | _ -> false


let load_json_common piqtype ast =
  let ast =
    if is_primitive piqtype
    then
    (* expecting primitive types to be wrapped in associative array because JSON
     * doesn't support them as top-level objects, according to RFC 4627 that
     * says: "A JSON text is a serialized object or array" *)
      match ast with
        | `Assoc [ "_", ast ] (* older pre- 0.6.0 format *)
        | `Assoc [ "value", ast ] -> ast
        | _ ->
            error ast
              "invalid toplevel value for primitive type: {\"value\": ...} expected"
    else if is_list piqtype
    then
      match ast with
        | `Assoc [ "value", ast ] -> ast (* sometimes top-level arrays are embedded in objects *)
        | _ -> ast
    else ast
  in
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = piqi_of_json ast in
    Piqi piqi
  else
    let obj = piqobj_of_json piqtype ast in
    Typed_piqobj obj


let load_json (user_piqtype: T.piqtype option) json_parser :obj =
  let ast = read_json_ast json_parser in
  (* check typenames, as Json parser doesn't do it unlike the Piq parser *)
  let check = true in
  match ast with
    | `Assoc (("piqi_type", `String "piqi") :: fields) ->
        let ast = Piqloc.addrefret ast (`Assoc fields) in
        (* NOTE: caching the loaded module *)
        let piqi = piqi_of_json ast ~process:false in
        Piqi piqi
    | `Assoc (("piqi_type", `String typename) :: fields) ->
        let piqtype = find_piqtype typename ~check in
        let ast = Piqloc.addrefret ast (`Assoc fields) in
        load_json_common piqtype ast
    | `Assoc (("piqi_type", ast) :: _) ->
        error ast "invalid \"piqi_type\" format"
    | _ ->
        (* there's no first field that looks like "piqi_type": ... => using the
         * user-supplied piqtype *)
        (match user_piqtype with
          | Some piqtype ->
              load_json_common piqtype ast
          | None ->
              C.error ast "default type for JSON object is unknown"
        )


(* load json while ignoring all embedded type hints *)
let load_plain_json (piqtype: T.piqtype) json_parser :obj =
  let ast = read_json_ast json_parser in
  let ast =
    match ast with
      | `Assoc (("piqi_type", `String _) :: fields) ->
          (* skip the "piqi_type" field whenever it is present *)
          Piqloc.addrefret ast (`Assoc fields)
      | _ ->
          ast
  in
  load_json_common piqtype ast


(*
 * XML reading and writing
 *)

let piqi_of_xml xml =
  let piqtype = !Piqi.piqi_spec_def in
  (* don't resolve defaults when reading xml *)
  let piqobj =
    C.with_resolve_defaults false (fun () -> Piqobj_of_xml.parse_obj piqtype xml)
  in
  (* don't try to track location references as we don't preserve them yet in
   * piqobj_of_xml *)
  Piqloc.pause ();
  let piqi = Piqi.piqi_of_piqobj piqobj in
  Piqloc.resume ();
  piqi


let piqi_to_xml piqi =
  let piqobj = Piqi.piqi_to_piqobj piqi in
  Piqobj_to_xml.gen_toplevel_obj piqobj


let gen_xml (obj :obj) :Piqi_xml.xml =
  match obj with
    | Typed_piqobj obj | Piqobj obj ->
        Piqobj_to_xml.gen_toplevel_obj obj
    | Piqi piqi ->
        (* output Piqi spec itself if we are converting .piqi *)
        piqi_to_xml piqi
    | Piqtype _ ->
        assert false (* type hints are not supported by xml encoding *)


let to_xml_channel ch (obj:obj) =
  let xml = gen_xml obj in
  Piqi_xml.xml_to_channel ch xml;
  Pervasives.output_char ch '\n'


let to_xml_string ?pretty_print obj =
  let xml = gen_xml obj in
  Piqi_xml.xml_to_string xml ?pretty_print


let read_xml_ast xml_parser :Piqi_xml.xml =
  let res = Piqi_xml.read_xml_obj xml_parser in
  match res with
    | Some ast -> ast
    | None -> raise EOF


let load_xml (piqtype: T.piqtype) xml_parser :obj =
  let ast = read_xml_ast xml_parser in
  if piqtype == !Piqi.piqi_lang_def (* XXX *)
  then
    let piqi = piqi_of_xml ast in
    Piqi piqi
  else
    let obj = Piqobj_of_xml.parse_obj piqtype ast in
    Typed_piqobj obj



let init () =
  (* XXX: this is necessary when we convert to/from json, but now calling it
   * regardless of whether we actually need it *)
  Piqi_json.init ()


(*
 * The converter:
 *)

let fname = "input" (* XXX *)


let load_piq_or_pib get_next ~is_piqi_input =
  let rec aux () =
    let obj = get_next () in
    match obj with
      | Piqtype _ -> aux () (* skip default type *)
      | Piqi _ when not is_piqi_input -> aux () (* skip embedded piqi *)
      | Piqobj obj -> Typed_piqobj obj
      | _ -> obj (* Typed_piqobj or Piqi *)
  in aux ()


let from_piq_string piqtype s ~is_piqi_input =
  let piq_parser = Piq_parser.init_from_string fname s in
  let get_next () = load_piq (Some piqtype) piq_parser in
  let obj = load_piq_or_pib get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let from_pib_string piqtype s ~is_piqi_input =
  let buf = Piqirun.IBuf.of_string s in
  let get_next () = load_pib (Some piqtype) buf in
  let obj = load_piq_or_pib get_next ~is_piqi_input in
  (* XXX: check eof? *)
  obj


let from_json_string piqtype s =
  let json_parser = Piqi_json_parser.init_from_string ~fname s in
  let obj = load_plain_json piqtype json_parser in
  (* XXX: check eof? *)
  obj


let from_pb_string piqtype s =
  let buf = Piqirun.init_from_string s in
  let obj = load_pb piqtype buf in
  (* XXX: check eof? *)
  obj


let from_xml_string piqtype s =
  let xml_parser = Piqi_xml.init_from_string ~fname s in
  let obj = load_xml piqtype xml_parser in
  (* XXX: check eof? *)
  obj


let parse_obj piqtype input_format data =
  (* XXX *)
  let is_piqi_input = (piqtype == !Piqi.piqi_lang_def) in
  let piqobj =
    match input_format with
      | `piq  -> from_piq_string piqtype data ~is_piqi_input
      | `json -> from_json_string piqtype data
      | `pb -> from_pb_string piqtype data
      | `xml -> from_xml_string piqtype data
      | `pib -> from_pib_string piqtype data ~is_piqi_input
  in piqobj


let gen_obj ~pretty_print output_format piqobj =
  match output_format with
    | `piq  -> to_piq_string piqobj
    | `json -> to_json_string piqobj ~pretty_print
    | `pb -> to_pb_string piqobj
    | `xml -> to_xml_string piqobj ~pretty_print
    | `pib -> to_pib_string piqobj


type options =
  {
    mutable json_omit_missing_fields : bool;
    mutable pretty_print : bool;
    mutable use_strict_parsing : bool;
    mutable piq_frameless_output : bool;
    mutable piq_frameless_input : bool;
    mutable piq_relaxed_parsing : bool;
  }


let make_options
        ?(pretty_print=true)
        ?(json_omit_missing_fields=true)
        ?(json_omit_null_fields=true) (* deprecated: use json_omit_missing_fields instead *)
        ?(use_strict_parsing=false)
        ?(piq_frameless_output=false)
        ?(piq_frameless_input=false)
        ?(piq_relaxed_parsing=false)
        () =
  {
    json_omit_missing_fields = json_omit_null_fields && json_omit_missing_fields;
    pretty_print = pretty_print;
    use_strict_parsing = use_strict_parsing;
    piq_frameless_output = piq_frameless_output;
    piq_frameless_input = piq_frameless_input;
    piq_relaxed_parsing = piq_relaxed_parsing;
  }


let set_options opts =
  Piqobj_to_json.omit_missing_fields := opts.json_omit_missing_fields;
  Piqi_config.flag_strict := opts.use_strict_parsing;
  Piqi_config.piq_frameless_output := opts.piq_frameless_output;
  Piqi_config.piq_frameless_input := opts.piq_frameless_input;
  Piqi_config.piq_relaxed_parsing := opts.piq_relaxed_parsing;
  ()


(* called by Piqirun_ext and Piqi_server *)
let find_type typename =
  if not (Piqi_name.is_valid_typename typename)
  then
    piqi_error ("invalid type name: " ^ typename);

  if typename = "piqi" (* special case *)
  then !Piqi.piqi_lang_def (* return Piqi type from embedded self-definition *)
  else
    try Piqi_db.find_piqtype typename
    with Not_found ->
      piqi_error ("unknown type: " ^ typename)


(* called by Piqirun_ext and Piqi_server *)
let convert ~opts piqtype input_format output_format data =

  (* apply some of the settings *)
  set_options opts;

  (* resetting source location tracking back to "enabled" state; we don't
   * carefully call matching Piqloc.resume () for every Piqloc.pause () if we
   * get exceptions in between *)
  Piqloc.is_paused := 0;

  (* perform the conversion
   * NOTE: we need to resolve all defaults before converting to JSON or XML *)
  let piqobj =
    C.with_resolve_defaults
      (output_format = `json || output_format = `xml)
      (fun () -> parse_obj piqtype input_format data)
  in
  (* reset location db to allow GC to collect previously read objects *)
  Piqloc.reset ();

  gen_obj output_format piqobj ~pretty_print:opts.pretty_print

