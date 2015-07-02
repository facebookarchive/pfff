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


(* command-line arguments *)
let input_encoding = ref ""
let output_encoding = ref ""
let typename = ref ""
let flag_add_defaults = ref false
let flag_embed_piqi = ref false
let flag_json_omit_missing_fields = ref true
let flag_piq_frameless_output = ref false
let flag_piq_frameless_input = ref false
let flag_piq_relaxed_parsing = ref false


let usage = "Usage: piqi convert [options] [input file] [output file]\nOptions:"

let arg_f =
    "-f", Arg.Set_string input_encoding,
    "pb|json|xml|piq|pib input encoding"

let arg_t =
    "-t", Arg.Set_string output_encoding,
    "pb|json|xml|piq|pib output encoding (piq is used by default)"

let arg__type =
    "--type", Arg.Set_string typename,
    "<typename> type of converted object"

let arg__add_defaults =
    "--add-defaults", Arg.Set flag_add_defaults,
    "add default field values to converted records"

let arg__embed_piqi =
    "--embed-piqi", Arg.Set flag_embed_piqi,
    "embed Piqi dependencies, i.e. Piqi specs which the input depends on"

let arg__json_omit_missing_fields =
    "--json-omit-missing-fields", Arg.Bool (fun x -> flag_json_omit_missing_fields := x),
    "true|false omit null and [] fields in JSON output (default=true)"

let arg__json_omit_null_fields =
    "--json-omit-null-fields", Arg.Bool (fun x -> flag_json_omit_missing_fields := x),
    "deprecated: use json-omit-missing-fields instead"

let arg__piq_frameless_output =
    "--piq-frameless-output", Arg.Bool (fun x -> flag_piq_frameless_output := x),
    "true|false print a frame around a single output Piq object (default=false)"

let arg__piq_frameless_input =
    "--piq-frameless-input", Arg.Bool (fun x -> flag_piq_frameless_input := x),
    "true|false expect a frame around a single input Piq object (default=false)"

let arg__piq_relaxed_parsing =
    "--piq-relaxed-parsing", Arg.Bool (fun x -> flag_piq_relaxed_parsing := x),
    "true|false parse Piq format using \"relaxed\" mode (default=false)"

let arg__gen_extended_piqi_any =
    "--gen-extended-piqi-any", Arg.Set Piqi_config.gen_extended_piqi_any,
    "use extended representation of piqi-any values in XML and JSON output"


let speclist = Main.common_speclist @
  [
    Main.arg__strict;
    Main.arg_o;

    arg_f;
    arg_t;
    arg__type;
    arg__add_defaults;
    arg__json_omit_missing_fields;
    arg__json_omit_null_fields;
    arg__piq_frameless_output;
    arg__piq_frameless_input;
    arg__piq_relaxed_parsing;
    arg__gen_extended_piqi_any;
    arg__embed_piqi;

    Main.arg__include_extension;
    Main.arg__;
  ]


let first_load = ref true

let load_piqi fname :Piqi_convert.obj =
  if !first_load
  then first_load := false
  else raise Piqi_convert.EOF; (* mimic the behaviour of Piqi_convert loaders *)

  (* NOTE, XXX: here also loading, processing and validating all the
   * module's dependencies *)
  let ch = Main.open_input fname in
  let piqi = Piqi.load_piqi fname ch in
  Piqi_convert.Piqi piqi


let resolve_typename () =
  if !typename <> ""
  then Some (Piqi_convert.find_type !typename)
  else None


(* ensuring that Piqi_convert.load_pb is called exactly one time *)
let load_pb piqtype protobuf :Piqi_convert.obj =
  if !first_load
  then first_load := false
  else raise Piqi_convert.EOF; (* XXX: print a warning if there are more input objects? *)
  Piqi_convert.load_pb piqtype protobuf


let first_write = ref true

let write_pb ch (obj: Piqi_convert.obj) =
  if !first_write
  then first_write := false
  else piqi_error "converting more than one object to \"pb\" is not allowed";
  Piqi_convert.to_pb_channel ch obj


let write_piq ch (obj: Piqi_convert.obj) =
  if !first_write
  then first_write := false
  else
    if !flag_piq_frameless_output
    then piqi_error "converting more than one object to frameless \"piq\" is not allowed";
  Piqi_convert.to_piq_channel ch obj


(* write only data and skip Piqi specifications and data hints *)
let write_data ~is_piqi_input writer ch (obj: Piqi_convert.obj) =
  match obj with
    | Piqi_convert.Piqi _ when not is_piqi_input ->
        (* ignore embedded Piqi specs if we are not converting .piqi *)
        ()
    | Piqi_convert.Piqtype _ ->
        (* ignore default type names *)
        ()
    | _ ->writer ch obj


(* write data and Piqi specifications and data hints *)
let write_data_and_piqi writer ch (obj: Piqi_convert.obj) =
  match obj with
    | Piqi_convert.Piqtype _ ->
        (* ignore default type names *)
        ()
    | _ -> writer ch obj


let make_reader load_f input_param =
  (fun () -> load_f input_param)


let make_reader input_encoding =
  let fname = !Main.ifile in
  let ch = Main.open_input fname in

  match input_encoding with
    | "pb" when !typename = "" ->
        piqi_error "--type parameter must be specified for \"pb\" input encoding"
    | "pb" ->
        let piqtype = some_of (resolve_typename ()) in
        let buf = Piqirun.init_from_channel ch in
        make_reader (load_pb piqtype) buf

    | "json" ->
        let json_parser = Piqi_json_parser.init_from_channel ch ~fname in
        make_reader (Piqi_convert.load_json (resolve_typename ())) json_parser

    | "xml" when !typename = "" ->
        piqi_error "--type parameter must be specified for \"xml\" input encoding"
    | "xml" ->
        let piqtype = some_of (resolve_typename ()) in
        let xml_parser = Piqi_xml.init_from_channel ch ~fname in
        make_reader (Piqi_convert.load_xml piqtype) xml_parser

    | "piq" ->
        let piq_parser = Piq_parser.init_from_channel fname ch in
        make_reader (Piqi_convert.load_piq (resolve_typename ())) piq_parser

    | "piqi" when !typename <> "" ->
        piqi_error "--type parameter is not applicable to \"piqi\" input encoding"
    | "piqi" ->
        make_reader load_piqi !Main.ifile

    | "pib" ->
        let buf = Piqirun.IBuf.of_channel ch in
        make_reader (Piqi_convert.load_pib (resolve_typename ())) buf
    | "" ->
        piqi_error "can't determine input encoding; use -f option to specify it explicitly"
    | x ->
        piqi_error ("unknown input encoding: " ^ U.quote x)


let make_writer ?(is_piqi_input=false) output_encoding =
  (* XXX: We need to resolve all defaults before converting to JSON or XML since
   * they are dynamic encoding, and it would be too unreliable and inefficient
   * to let a consumer decide what a default value for a field should be in case
   * if the field is missing. *)
  C.resolve_defaults := !flag_add_defaults ||
    (match output_encoding with
      | "json" | "xml" -> true
      | _ -> false);
  match output_encoding with
    | "" (* default output encoding is "piq" *)
    | "piq" -> write_piq
    | "pib" -> Piqi_convert.to_pib_channel
    | "json" ->
        write_data_and_piqi Piqi_convert.to_json_channel
    | "pb" ->
        write_data write_pb ~is_piqi_input
    | "xml" ->
        write_data Piqi_convert.to_xml_channel ~is_piqi_input
    | x ->
        piqi_error ("unknown output encoding " ^ U.quote x)


let seen = ref [] (* the list of seen elements *)

let is_seen x = List.memq x !seen

let add_seen x = seen := x :: !seen

let check_update_unseen x =
  let is_unseen = not (is_seen x) in
  (* add unseen element to the list of seen ones *)
  if is_unseen then add_seen x;
  is_unseen (* return true for yet unseen elements *)

let remove_update_seen l =
  List.filter check_update_unseen l


let rec get_piqi_deps piqi ~only_imports =
  if Piqi.is_boot_piqi piqi
  then[] (* boot Piqi (a parent of built-in types) is not a dependency *)
  else Piqi.get_piqi_deps piqi ~only_imports


let get_parent_piqi (t: T.piqtype) =
  let typedef =
    match t with
      | #T.typedef as x -> x
      | _ -> assert false
  in
  C.get_parent_piqi typedef


let get_dependencies (obj :Piqi_convert.obj) ~only_imports =
  let deps =
    match obj with
      | Piqi_convert.Piqi piqi ->
          (* add piqi itself to the list of seen *)
          add_seen piqi;
          get_piqi_deps piqi ~only_imports
      | _ -> (
          let piqtype =
            match obj with
              | Piqi_convert.Piqtype name ->
                  Piqi_db.find_piqtype name
              | Piqi_convert.Typed_piqobj obj | Piqi_convert.Piqobj obj ->
                  Piqobj_common.type_of obj
              | _ -> assert false
          in
          let piqi = get_parent_piqi piqtype in
          (* get dependencies for yet unseen (and not yet embedded) piqi *)
          if is_seen piqi
          then []
          else get_piqi_deps piqi ~only_imports
      )
  in
  (* filter out already seen deps along with updating the list of seen deps *)
  remove_update_seen deps


let validate_options input_encoding =
  let typename_str =
    if !typename = ""
    then ""
    else "values of type " ^ U.quote !typename ^ " "
  in
  let output_encoding_str =
    if !output_encoding = ""
    then "piq" (* default output encoding is "piq" *)
    else !output_encoding
  in
  trace "converting %sfrom .%s to .%s\n" typename_str input_encoding output_encoding_str;

  if !flag_embed_piqi
  then (
    match !output_encoding with
      | "pb" | "xml" ->
          if input_encoding = "piqi"
          then
            piqi_error "can't --embed-piqi when converting .piqi to .pb or .xml"
          else
            piqi_warning "--embed-piqi doesn't have any effect when converting to .pb or .xml"
      | _ -> ()
  )


let do_convert ?writer ?(is_piq_output=false) reader =
  (* read next object to the input channel *)
  let read_obj () =
    try
      let obj = reader () in
      Some obj
    with
      Piqi_convert.EOF -> None
  in
  (* write the object to the output channel *)
  let do_write_obj obj =
    match writer with
      | None -> ()
      | Some f -> f obj
  in
  (* write the object to the output, possibly including its Piqi dependencies *)
  let write_obj obj =
    (* write object's Piqi dependencies *)
    if !flag_embed_piqi
    then (
      (* write yet unwirtten object's dependencies *)
      let deps = get_dependencies obj ~only_imports:(not is_piq_output) in
      List.iter (fun x ->
        trace "piqi convert: embedding dependency module %s\n" (some_of x.P.modname);
        do_write_obj (Piqi_convert.Piqi x)
      ) deps
    );
    (* finally, write the object itself *)
    do_write_obj obj
  in
  let write_piqi piqi_list =
    Piqi_convert.process_unprocessed_piqi ();
    List.iter (fun modname ->
      let piqi = Piqi_db.find_piqi modname in
      trace "piqi convert: writing module %s\n" modname;
      write_obj (Piqi_convert.Piqi piqi)
    )
    (List.rev piqi_list)
  in
  let rec aux piqi_list =
    (* resetting source location tracking back to "enabled" state; we don't
     * carefully call matching Piqloc.resume () for every Piqloc.pause () if we
     * get exceptions in between *)
    Piqloc.is_paused := 0;
    match read_obj () with
      | None ->
          (* flush all yet unwritten piqi modules at EOF *)
          write_piqi piqi_list
      | Some obj ->
          (* reset location db to allow GC collect previously read
           * objects *)
          Piqloc.reset ();
          let piqi_list =
            match obj with
              | Piqi_convert.Piqi piqi when not (Piqi.is_processed piqi) ->
                  let modname = some_of piqi.P.modname in
                  modname :: piqi_list (* add to the list of unprocessed modules *)
              | _ ->
                  (* once we read a non-piqi object, flush all yet unwritten
                   * piqi modules *)
                  write_piqi piqi_list;
                  (* finally write the object we've just read *)
                  write_obj obj;
                  (* return empty list as a new value of piqi_list *)
                  []
          in
          aux piqi_list
  in
  aux []


let convert_file () =
  Piqi_convert.init ();
  Piqi_convert.set_options
    (Piqi_convert.make_options
      ~json_omit_missing_fields:!flag_json_omit_missing_fields
      ~piq_frameless_output:!flag_piq_frameless_output
      ~piq_frameless_input:!flag_piq_frameless_input
      ~piq_relaxed_parsing:!flag_piq_relaxed_parsing
      ~use_strict_parsing:!Piqi_config.flag_strict
      ()
    );
  let input_encoding =
    if !input_encoding <> ""
    then !input_encoding
    else Piqi_file.get_extension !Main.ifile
  in
  validate_options input_encoding;

  let reader = make_reader input_encoding in
  let is_piqi_input = (input_encoding = "piqi" || !typename = "piqi") in
  let writer = make_writer !output_encoding ~is_piqi_input in
  (* open output file *)
  let ofile =
    match !Main.ofile with
      | "" when !output_encoding = "" -> "" (* print "piq" to stdout by default *)
      | "" when !Main.ifile <> "" && !Main.ifile <> "-" ->
          let output_extension = !output_encoding in
          !Main.ifile ^ "." ^ output_extension
      | x -> x
  in
  let och = Main.open_output ofile in

  let is_piq_output =
    match !output_encoding with
      | "" | "piq" -> true
      | _ -> false
  in
  (* main convert cycle *)
  trace "piqi convert: main loop\n";
  do_convert reader ~writer:(writer och) ~is_piq_output


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  convert_file ()

 
let _ =
  Main.register_command run "convert"
    "convert data files between various encodings (pb, json, xml, piq, pib)"

