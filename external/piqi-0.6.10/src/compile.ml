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

(* use self-spec to compile %.piqi into a portable piqi-list *)


module C = Piqi_common
open C


(* command-line arguments *)
let input_format = ref ""
let output_format = ref ""
let input_self_spec = ref ""


(* NOTE: "piqi compile" command-line interface should be idealy as stable
 * (backward-compatible) as as the Piqi self-spec; this way users can rely on
 * the tool without closely tracking piqi releases *)
let usage = "Usage: piqi compile [options] <.piqi file>\nOptions:"

let arg_f =
  "-f", Arg.Set_string input_format,
  "pb|json|xml|piqi input format (default=piqi)"

let arg_t =
  "-t", Arg.Set_string output_format,
  "pb|json|xml|piq|piqi output format (default=piq)"

(* The reason we require self-spec to be in .pb format is because it is faster
 * to parse it this way, not because it is more portable than any other format.
 * Also, it is better to have a deterministic interface when we expect input in
 * only one format -- we don't want to autodetect it when reading from stdin *)
let arg__self_spec =
  "--self-spec", Arg.Set_string input_self_spec,
  "<.pb file> input self-spec in .pb format; use '-' for stdin"


let speclist = Piqi_compile.getopt_speclist @
  [
    Main.arg_o;
    arg_f;
    arg_t;
    arg__self_spec;
  ]


let compile self_spec piqi och =
  trace "getting all imported dependencies\n";
  let piqi_list =
    if !output_format <> "piqi"
    then Piqi.get_piqi_deps piqi
    else [piqi] (* we want to output just this module alone *)
  in
  (* get necessary piqtypes from the self-spec *)
  let filename = !input_self_spec in
  let piqi_piqtype =
    if self_spec == C.some_of !Piqi.piqi_spec  (* is it the default embedded self-spec? *)
    then None
    else Some (Piqi_compile.get_self_spec_piqtype self_spec "piqi" ~filename)
  in
  let piqi_list_piqtype = Piqi_compile.get_self_spec_piqtype self_spec "piqi-list" ~filename in

  trace "converting modules to internal representation\n";
  (* We need to resolve all defaults before converting to JSON or XML because
   * they are dynamic encoding and their de-serializers no notion of default
   * values *)
  C.resolve_defaults := (match !output_format with
      | "json" | "xml" -> true
      | _ -> false);
  Config.flag_strict := !Piqi_compile.flag_strict;

  (* convert all modules to internal representation *)
  let piqobj_list = List.map
    (fun piqi -> Piqi.piqi_to_piqobj piqi ?piqi_piqtype ~add_codes:true)
    piqi_list
  in
  trace "writing output\n";
  match !output_format with
    | "piq" | "" ->
        (* NOTE: instead of creating piqi-list, writing modules in regular .piq
         * notation *)
        let write_piq piqobj =
          let ast = Piqi_convert.gen_piq (Piqi_convert.Piqobj piqobj) in
          let ast = Piqi_convert.piqi_ast_to_piq ast in
          Piq_gen.to_channel och ast;
          Pervasives.output_char och '\n'
        in
        List.iter write_piq piqobj_list
    | "piqi" ->
        (* NOTE: with output_format = "piqi" we can output only one modele --
         * the one that's beeing compiled; it comes last in the list *)
        let piqobj = List.hd (List.rev piqobj_list) in
        let ast = Piqi_convert.gen_piq (Piqi_convert.Piqobj piqobj) in
        Piqi_pp.prettyprint_piqi_ast och ast
    | format ->
        let writer =
          match format with
            | "json" -> Piqi_convert.to_json_channel
            | "pb" -> Piqi_convert.to_pb_channel
            | "xml" -> Piqi_convert.to_xml_channel
            | x -> piqi_error ("unknown output format " ^ U.quote x)
        in
        let piqobj = Piqi_compile.make_piqi_list_piqobj piqi_list_piqtype piqobj_list in
        writer och (Piqi_convert.Piqobj piqobj)


let load_self_spec () =
  let ifile = !input_self_spec in
  if ifile <> "" (* regular compilation mode mode with explicit --self-spec *)
  then (
    let ich = Main.open_input ifile in
    let buf = Piqirun.init_from_channel ich in
    let piqi = Piqi_compile.load_self_spec buf ~filename:ifile in
    Main.close_input ();
    piqi
  )
  else (
    trace "--self-spec argument is missing; using the default embedded self-spec to compile\n";
    C.some_of !Piqi.piqi_spec
  )


let load_piqi_piqobj input_format piqi_piqtype fname ch =
  match input_format with
    | "pb" ->
        let protobuf = Piqirun.init_from_channel ch in
        (* don't store location references as we're loading from the binary object *)
        Piqloc.pause ();
        let piqobj = Piqobj_of_protobuf.parse_obj piqi_piqtype protobuf in
        Piqloc.resume ();
        piqobj
    | "json" ->
        let json_parser = Piqi_json_parser.init_from_channel ch ~fname in
        let json = Piqi_convert.read_json_ast json_parser in
        Piqobj_of_json.parse_obj piqi_piqtype json
    | "xml" ->
        let xml_parser = Piqi_xml.init_from_channel ch ~fname in
        let xml = Piqi_convert.read_xml_ast xml_parser in
        Piqobj_of_xml.parse_obj piqi_piqtype xml
    | x ->
        piqi_error ("unknown input format " ^ U.quote x)


let load_piqi self_spec fname =
  let ch = Main.open_input fname in
  match !input_format with
    | "piqi" | "" ->
        Piqi.load_piqi fname ch
    | other_format ->
        let piqi_piqtype = Piqi_compile.get_self_spec_piqtype self_spec "piqi" ~filename:!input_self_spec in
        (* don't resolve defaults *)
        let piqobj =
          C.with_resolve_defaults false (
            fun () -> load_piqi_piqobj other_format piqi_piqtype fname ch
          )
        in
        (* TODO: don't try to track location references as we don't preserve
         * them yet in xml, json or pb *)
        Piqloc.pause ();
        let piqi = Piqi.piqi_of_piqobj piqobj in
        Piqloc.resume ();
        piqi


let run_compile () =
  let ifile = !Main.ifile in

  let self_spec = load_self_spec () in
  let piqi = load_piqi self_spec ifile in

  let och = Main.open_output !Main.ofile in
  compile self_spec piqi och


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1;

  Piqi_json.init (); (* we need it for converting to JSON *)

  (* always generate extended piqi any; the goal is to standardise on the
   * representation, make command-line interface simpler and don't give users
   * unnecessary choices *)
  Piqi_config.gen_extended_piqi_any := true;

  run_compile ()


let _ =
  Main.register_command run "compile" "use self-spec to compile %.piqi into a portable piqi-list"

