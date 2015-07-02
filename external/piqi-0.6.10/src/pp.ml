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


open Piqi_common 


(* command-line parameters *)
let flag_normalize = ref false
let flag_expand_abbr = ref false
let flag_parse_literals = ref false


let open_piq fname =
  let ch = Main.open_input fname in
  let piq_parser = Piq_parser.init_from_channel fname ch in
  piq_parser


let read_piq_obj piq_parser =
  let res = Piq_parser.read_next piq_parser ~expand_abbr:!flag_expand_abbr in
  (* reset location db to allow GC to collect previously read objects *)
  Piqloc.reset ();
  res


let normalize_ast ast =
  Piq_ast.map_words ast Piqi_name.normalize_name


let transform_ast ast =
  if !flag_normalize
  then normalize_ast ast
  else ast


let prettyprint_piq ch piq_parser =
  Piqi_config.piq_relaxed_parsing := !Convert.flag_piq_relaxed_parsing;

  let rec aux () =
    match read_piq_obj piq_parser with
      | None -> ()
      | Some ast ->
          let ast = transform_ast ast in
          Piqi_pp.prettyprint_ast ch ast;
          output_char ch '\n';
          aux ()
  in aux ()


let prettyprint_file filename =
  let ch = Main.open_output !Main.ofile in

  let piq_parser = open_piq filename in
  prettyprint_piq ch piq_parser


let usage = "Usage: piqi pp [options] [<.piqi|.piq file>] [output file]\nOptions:"


let speclist = Main.common_speclist @
  [
    Main.arg_o;

    "--normalize-words", Arg.Set flag_normalize,
    "normalize all words while pretty-printing (convert CamelCase to camel-case)";

    "--expand-abbr", Arg.Set flag_expand_abbr,
    "expand built-in syntax abbreviations";

    "--parse-literals", Arg.Set flag_parse_literals,
    "deprecated: this option has no effect";

    Convert.arg__piq_relaxed_parsing;

    Main.arg__;
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  prettyprint_file !Main.ifile

 
let _ =
  Main.register_command run "pp" "pretty-print %.piqi or %.piq"

