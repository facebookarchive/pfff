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
 * utilities for running piqi in command-line mode
 *)


module C = Piqi_common


(* command-line parameters *)
let odir = ref ""
let ifile = ref ""
let ofile = ref ""


let flag_keep_tmp_files = ref false


let ich = ref stdin
let och = ref stdout


let open_input = function
  | "-" | "" -> stdin
  | f ->
      try
        let c = open_in_bin f in
        ifile := f;
        ich := c; c
      with Sys_error s ->
        C.piqi_error ("failed to open input file: " ^ s)


let open_output = function
  | "-" | "" -> stdout
  | f ->
      try 
        let c = open_out_bin f in
        ofile := f;
        och := c; c
      with Sys_error s ->
        C.piqi_error ("failed to open output file: " ^ s)


(* close previously opened output channel *)
let close_output () =
  if !och != stdout
  then close_out !och


(* close previously opened input channel *)
let close_input () =
  if !ich != stdin
  then close_in !ich


let tmp_files = ref []

let add_tmp_file (f:string) =
  tmp_files := f::!tmp_files


let delete_file fname =
  try Sys.remove fname
  with _ -> ()


let cleanup () =
  close_input ();
  close_output ();

  (* remove temporary files *)
  if not !flag_keep_tmp_files
  then List.iter delete_file !tmp_files;
  ()


let cleanup_on_error () =
  (try cleanup () with _ -> ());
  (* delete output file *)
  if !och != stdout
  then delete_file !ofile;
  ()


let chdir_output dir =
  try
    begin
      match dir with 
        | "" -> ()
        | _ -> Sys.chdir dir
    end
  with Sys_error s ->
    C.piqi_error ("failed to chdir to output directory: " ^ dir)


let arg_count = ref 0

let default_anon_fun s =
  match !arg_count with
    | 1 ->
        (* the first positional argument is input file *)
        ifile := s
    | 2 ->
        (* the second positional argument is output file unless overriden by -o
         * option *)
        if !ofile = ""
        then ofile := s
    | _ -> ()


let anon_fun_wrapper f s =
  incr arg_count;
  f s


let arg_I =
  "-I", Arg.String Piqi_config.add_path,
    "<dir> add directory to the list of imported .piqi search paths"

let arg_o =
  "-o", Arg.Set_string ofile,
    "<output file> specify output file; use '-' for stdout"

let arg_C =
  "-C", Arg.Set_string odir,
    "<output directory> specify output directory"

let arg__ =
   "--", Arg.Rest (anon_fun_wrapper default_anon_fun),
     "supply other arguments possibly including '-' for stdin input/output"

let arg__keep_tmp_files =
   "--keep-tmp-files", Arg.Set flag_keep_tmp_files,
     "don't delete temporary files created during command execution"

let arg__no_warnings =
   "--no-warnings", Arg.Set Piqi_config.flag_no_warnings,
     "don't print warnings"

let arg__trace =
   "--trace", Arg.Set Piqi_config.flag_trace,
     "turn on tracing"

let arg__debug =
   "--debug", Arg.Set_int Piqi_config.debug_level,
     "<level> debug level; any number greater than 0 turns on debug messages"

let arg__no_builtin_types =
   "--no-builtin-types", Arg.Set Piqi_config.flag_no_builtin_types,
     "don't include built-in type definitions while processing .piqi"

let arg__strict =
   "--strict", Arg.Set Piqi_config.flag_strict,
     "treat unknown and duplicate fields as errors"

let arg__include_extension =
   "-e", Arg.String Piqi_config.add_include_extension,
     "<name> try including extension <name> for all loaded modules (can be used several times)"


let common_speclist =
  [
   arg_I;
   arg__no_warnings;
   arg__trace;
   arg__debug;
   arg__no_builtin_types;
  ]


let parse_args
    ~speclist ~usage
    ?(min_arg_count=1) ?(max_arg_count=1) ?(anon_fun=default_anon_fun) () =
  Arg.parse speclist (anon_fun_wrapper anon_fun) usage;
  if !arg_count < min_arg_count || !arg_count > max_arg_count
  then (
    Arg.usage speclist usage;
    exit 3;
  );
  (* append CWD and $PIQI_PATH the list of search paths provided by -I .. *)
  Piqi_config.init_paths ()


let die s =
  prerr_endline s;
  exit 1


let run_command f =
  (* init various stuff related to --debug <level> *)
  Piqi_config.init_debug ();

  try
    f ();
    cleanup ();
  with
    | C.Piqi_error s ->
        cleanup_on_error ();
        die s
    | Piqi_common.Error (loc, s) ->
        cleanup_on_error ();
        die (Piqi_common.strerr loc s)
    | Sys_error s ->
        cleanup_on_error ();
        die ("uncaught system error: " ^ s)

