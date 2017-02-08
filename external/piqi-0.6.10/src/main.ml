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


(* interfaces for defining sub-command *)
type command =
  {
    name : string;
    descr : string;
    run : (unit -> unit);
  }


let commands :command list ref = ref []


let register_command f name descr =
  let cmd = { name = name; descr = descr; run = f } in
  commands := cmd :: !commands


let find_command name =
    List.find (function x when x.name = name -> true | _ -> false) !commands


let usage () =
  prerr_endline "\
Usage: piqi <command> [<command-options>]
       piqi [--version]

Commands:";

  List.iter
    (fun cmd -> Printf.eprintf "  %-15s%s\n" cmd.name cmd.descr)
    (List.rev !commands);

prerr_endline "\n\
See 'piqi <command> --help' for more information on a specific command.

Options:
  --version  Print Piqi version and exit\n"


let exit_usage () =
  usage ();
  exit 2


let run_subcommand argv1 =
  match argv1 with
    | "--version" ->
        print_endline Piqi_version.version
    | "help" | "--help" | "-h" ->
        usage ()
    | _ ->
        (* find command by command name passed as the first argument *)
        let cmd =
          try find_command argv1
          with Not_found ->
            exit_usage ()
        in
        Arg.current := 1; (* subcommand -- skip argv[0] *)
        Piqi_command.run_command cmd.run


(* called by Piqi_run *)
let run () =
  if !Sys.interactive
  then () (* don't do anything in interactive (toplevel) mode *)
  else
    if Array.length Sys.argv < 2
    then exit_usage ()
    else run_subcommand Sys.argv.(1)


(* include commonly used functions in the module's namespace *)
include Piqi_command

