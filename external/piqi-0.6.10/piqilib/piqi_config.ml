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


(* .piqi search paths *)
let paths = ref []


let add_path x =
  paths := !paths @ [x]


let piqi_path =
  try
    let s = Sys.getenv "PIQI_PATH" in
    let sep =
      match Sys.os_type with
        | "Win32" -> ';'
        | _ -> ':'
    in
    let l = Piqi_util.string_split s sep in
    List.filter (fun s -> s <> "") l (* remove empty segments *)
  with
    Not_found -> []


(* set .piqi search path to contain CWD and $PIQI_PATH *)
let init_paths () =
  paths := !paths @ ("." :: piqi_path)


let reset_paths () =
  paths := []


(*
 * command-line options 
 *)


(* don't include built-in type definitions into piqi specifications that are
 * being processed *)
let flag_no_builtin_types = ref false


let flag_strict = ref false
let flag_no_warnings = ref false
let debug_level = ref 0
let flag_trace =
  try 
    ignore (Sys.getenv "PIQI_TRACE");
    ref true
  with Not_found ->
    ref false


let init_debug () =
  (* check location DB consistency on all debug levels *)
  if !debug_level >= 1 then Piqloc.check := true;
  (* turn on extra debugging about source line number tracking *)
  if !debug_level >= 3 then Piqloc.trace := true;
  if !debug_level >= 4 then Piqloc.crash_on_error := true;
  ()


(* this variable controls whether we parse and generate piq AST
 * for/during pretty-printing or for real use *)
let pp_mode = ref false


(* Piqi extensions automatically included when loading modules *)
let extensions = ref []

let add_include_extension (name :string) =
  extensions := !extensions @ [ name ]


(* for JSON and XML output: whether to generate piqi-any values using symbolic
 * JSON and/or XML representation (this is the default) or use full piqi-any
 * representation that wraps JSON or XML symbolic representation in a record
 * that includes the value itself, plus protobuf representation of the value,
 * typename and possibly something else *)
let gen_extended_piqi_any = ref false


(* whether to print a frame around a single output piq object *)
let piq_frameless_output = ref false
(* whether to expect a frame around a single input piq object *)
let piq_frameless_input = ref false


(* whether to parse piq in "relaxed" mode, e.g. treat words as string literals
 * and some other convenient stuff; for instance, piqi_getopt indirectly uses
 * relaxed parsing mode *)
let piq_relaxed_parsing = ref false

