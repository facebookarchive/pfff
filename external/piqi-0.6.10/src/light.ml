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
 * Light syntax for Piqi DDL: Piqi to Piqi-light pretty-printer
 *)


let print_piqi_file () =
  let ich = Main.open_input !Main.ifile in
  let piqi = Piqi.load_piqi !Main.ifile ich in

  let och = Main.open_output !Main.ofile in
  Piqi_light.gen_piqi och piqi


let usage = "Usage: piqi light [options] [<.piqi file>] [output-file]\nOptions:"

let speclist = Main.common_speclist @
  [
    Main.arg_o;
  ]


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:2;
  print_piqi_file ()


let _ =
  Main.register_command run "light"
    "pretty-print %.piqi using Piqi-light syntax"

