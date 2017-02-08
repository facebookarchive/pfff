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
let flag_includes_only = ref false
let flag_extensions = ref true
let flag_functions = ref false
let flag_all = ref false
let flag_add_module_name = ref false

let usage = "Usage: piqi expand [options] <.piqi file> [output file]\nOptions:"

let speclist = Main.common_speclist @
  [
    Main.arg_o;

   "--includes-only", Arg.Set flag_includes_only,
     "expand only includes";

   "--extensions", Arg.Set flag_extensions,
     "expand extensions in additon to includes (this is the default option)";

   "--functions", Arg.Set flag_functions,
     "expand functions in additon to includes";

   "--all", Arg.Set flag_all,
     "same as specifying --extensions --functions";

   "--add-module-name", Arg.Set flag_add_module_name,
     "add module name if it wasn't originally present";

    Main.arg__strict;
    Main.arg__include_extension;
  ]


let expand_file () =
  let ich = Main.open_input !Main.ifile in
  let piqi = Piqi.load_piqi !Main.ifile ich in

  if !flag_functions
  then flag_extensions := false;

  if !flag_includes_only
  then (flag_extensions := false; flag_functions := false);

  if !flag_all
  then (flag_extensions := true; flag_functions := true);

  let res_piqi = Piqi.expand_piqi piqi
        ~extensions:!flag_extensions
        ~functions:!flag_functions
  in

  if !flag_add_module_name && res_piqi.P.modname = None
  then res_piqi.P.modname <- piqi.P.modname;

  let och = Main.open_output !Main.ofile in
  Piqi_pp.prettyprint_piqi och res_piqi


let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:1 ~max_arg_count:2;
  expand_file ()

 
let _ =
  Main.register_command run "expand"
    "expand %.piqi includes and extensions"

