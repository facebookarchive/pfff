(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

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
 * Piqi compiler for OCaml
 *)

module C = Piqic_common
open C
open Iolist


(* command-line flags *)
let flag_pp = ref false
let flag_gen_defaults = ref false  (* deprecated -- always enabled by default *)
let flag_embed_piqi = ref false
let flag_multi_format = ref false
let flag_runtime = ref ""
let flag_version = ref false
let flag_piqi_version = ref false


let arg__pp =
  "--pp", Arg.Set flag_pp,
    "(DEPRECATED) pretty-print output using CamlP4 (camlp4o)"

let arg__normalize_names =
  "--normalize-names", Arg.Bool (fun x -> C.flag_normalize_names := x),
    "true|false turn CamlCase-style names into \"camel_case\" (default = true)"

let arg__gen_defaults =
  "--gen-defaults", Arg.Set flag_gen_defaults,
    "(depreacted) always enabled: generate default value constructors for generated types"

let arg__gen_preserve_unknown_fields =
  "--gen-preserve-unknown-fields", Arg.Set C.flag_gen_preserve_unknown_fields,
    "generate code that preserve unknown Protobuf fields when they are serialized back"

let arg__embed_piqi =
  "--embed-piqi", Arg.Set flag_embed_piqi,
    "embed Piqi modules encoded in binary format in the generated code"

let arg__multi_format =
  "--multi-format", Arg.Set flag_multi_format,
    "generate extended OCaml stubs for multi-format (JSON/XML/Piq/Pb) serialization"

let arg__ext =
  "--ext", Arg.Set flag_multi_format,
    "same as --multi-format"

let arg__runtime =
  "--runtime", Arg.Set_string flag_runtime,
    "<module> name of the Protobuf serialization runtime module (default = Piqirun)"

let arg__cc =
  "--cc", Arg.Set C.flag_cc,
    "compiler compiler mode -- used only for building piqilib"

let arg__version =
  "--version", Arg.Set flag_version,
    "print piqi-ocaml version and exit"

let arg__piqi_version =
  "--piqi-version", Arg.Set flag_piqi_version,
    "print piqi version and exit"


let ocaml_pretty_print ifile ofile =
  (* NOTE: we need "-printer o", because Camlp4o uses the Camlp4AutoPrinter by
   * default (no -printer argument provided), which will either produce ocaml
   * code or a binary AST depending on whether the output is a terminal or not
   * (regardless of any -o option provided). started on a terminal, the results
   * of piqic-ocaml --pp was a binary AST file *)
  let cmd = Printf.sprintf "camlp4o -printer o -o %s %s" ofile ifile in
  let res = Sys.command cmd in
  if res <> 0
  then C.error ("command execution failed: " ^ cmd)


let gen_output_file ofile code =
  if not !flag_pp
  then
    let ch = Piqi_command.open_output ofile in
    Iolist.to_channel ch code;
    Piqi_command.close_output ()
  else
    begin
      (* prettyprint generated OCaml code using Camlp4 *)
      let tmp_file = ofile ^ ".tmp.ml" in
      (try
        let tmp_ch = open_out tmp_file in
        Iolist.to_channel tmp_ch code;
        close_out tmp_ch;
      with Sys_error s ->
        C.error ("error writing temporary file: " ^ s));
      ocaml_pretty_print tmp_file ofile;
      Piqi_command.add_tmp_file tmp_file;
    end


(* build a list of all import dependencies including the specified module and
 * encode each Piqi module in the list using Protobuf encoding *)
let gen_embedded_piqi piqi_list =
  let piqi = List.hd (List.rev piqi_list) in
  let s = Piqirun.to_string (T.gen_piqi piqi) in
  iol [
    ios "let piqi = "; ioq (String.escaped s); eol
  ]


let gen_custom_runtime () =
  if !flag_runtime <> ""
  then iol [ios "module Piqirun = "; ios !flag_runtime; eol; eol]
  else iol []


let gen_piqi_ml context =
  let modname = C.top_modname context in
  let code = iol [
    gen_custom_runtime ();

    Piqic_ocaml_types.gen_piqi context;
    Piqic_ocaml_in.gen_piqi context;
    Piqic_ocaml_out.gen_piqi context;
    Piqic_ocaml_defaults.gen_piqi context;

    (* NOTE: --multi-format serialization depends on --embded-piqi *)
    if !flag_embed_piqi || !flag_multi_format
    then gen_embedded_piqi context.modules
    else iol [];

    ios "include "; ios modname; eol;
  ]
  in
  let ofile = String.uncapitalize modname ^ ".ml" in
  gen_output_file ofile code


let gen_piqi_ext_ml context =
  let code = iol [
    gen_custom_runtime ();
    Piqic_ocaml_ext.gen_piqi context;
  ]
  in
  let modname = C.top_modname context in
  let ofile = String.uncapitalize modname ^ "_ext.ml" in

  gen_output_file ofile code


let piqic context =
  (* chdir to the output directory *)
  Piqi_command.chdir_output !Piqi_command.odir;

  gen_piqi_ml context;

  if !flag_multi_format
  then gen_piqi_ext_ml context


(* this is the same as calling "piqi compile ..." and reading its output but,
 * instead, we are just using the library functions *)
let piqi_compile_piqi ifile =
  let self_spec_bin = T.piqi in
  (* by adding "ocaml" extension, we tell the library to automatically load
   * *.ocaml.piqi extension modules *)
  Piqi_compile.compile self_spec_bin ifile ~extensions:["ocaml"]


let load_piqi_list ifile =
  let bin = piqi_compile_piqi ifile in
  (* read the compiled piqi bundle *)
  let buf = Piqirun.init_from_string bin in
  let bundle = T.parse_piqi_bundle buf in
  (* return the list of piqi modules: list of dependencies @ [input module] *)
  bundle.T.Piqi_bundle.piqi


let piqic_file ifile =
  (* load input .piqi file and its dependencies *)
  let piqi_list = load_piqi_list ifile in
  let context = C.init piqi_list in
  piqic context


let speclist = Piqi_compile.getopt_speclist @
  [
    Piqi_command.arg_C;
    arg__normalize_names;
    arg__pp;
    arg__gen_defaults;
    arg__gen_preserve_unknown_fields;
    (* TODO: deprecated and remove in the next major release (together with --pp) *)
    Piqi_command.arg__keep_tmp_files;
    arg__embed_piqi;
    arg__multi_format;
    arg__ext;
    arg__runtime;
    arg__cc;
    arg__version;
    arg__piqi_version;
  ]


let usage = "\
Usage: piqic-ocaml [options] <.piqi file>
       piqic-ocaml [--version | --piqi-version]

Options:"


let print_and_exit s =
  print_endline s;
  exit 0


let run () =
  (* handle --version and --piqi-version; doing it separately from the rest of
   * args, because normally we expect one positional arg and this is
   * automatically handled by Piqi_command.parse_args () *)
  let argv1 =
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else ""
  in
  if argv1 = "--version"
  then print_and_exit Piqic_ocaml_version.version
  else if argv1 = "--piqi-version"
  then print_and_exit Piqi_version.version;

  (* normal invocation *)
  Piqi_command.parse_args () ~usage ~speclist;

  if !flag_gen_defaults
  then C.warning "--gen-defaults flag is deprecated: always generating defaults";

  if !flag_pp
  then C.warning "--pp flag is deprecated (generated code is now pretty-printed by default)";

  (match Piqi_command.arg__keep_tmp_files with
    | _, Arg.Set keep_tmp_files, _ ->
        if !keep_tmp_files
        then C.warning "--keep-tmp-files flag is deprecated (useless without deprecated --pp)";
    | _ -> ()
  );

  piqic_file !Piqi_command.ifile


let _ =
  Piqi_command.run_command run

