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


(* This module implements a Piqi tools server. It reads commands from stdin and
 * writes results to stdout.
 *
 * Piqi_call is an example of a client that uses this interface *)


module C = Piqi_common
open C

module I = Piqi_tools_piqi


(* utility functions for constructing Piqi_rpc responses *)
let return_ok_empty () =
  `ok_empty

let return_ok data =
  let data = Piqirun.to_string data in
  `ok data

let return_error data =
  let data = Piqirun.to_string data in
  `error data

let return_rpc_error err =
  `rpc_error err


(* mutable placeholder for the current convert options; we use it to avoid
 * allocation of a new structure on each call *)
let convert_options = Piqi_convert.make_options ()


let set_convert_options args =
  let open I.Convert_input in (
    convert_options.Piqi_convert.pretty_print <- args.pretty_print;
    convert_options.Piqi_convert.json_omit_missing_fields <- args.json_omit_missing_fields;
    convert_options.Piqi_convert.use_strict_parsing <- args.use_strict_parsing;
    convert_options.Piqi_convert.piq_frameless_output <- args.piq_frameless_output;
    convert_options.Piqi_convert.piq_frameless_input <- args.piq_frameless_input;
    convert_options.Piqi_convert.piq_relaxed_parsing <- args.piq_relaxed_parsing;
  )


(* "convert" call handler *)
let convert args =
  let open I.Convert_input in
  let piqtype = Piqi_convert.find_type args.type_name in
  set_convert_options args;
  let output = Piqi_convert.convert
      piqtype args.input_format args.output_format args.data
      ~opts:convert_options
  in
  `ok I.Convert_output.({data = output})


(* common error handler *)
let with_handle_piqi_errors f x =
  try f x
  with
    | C.Error (loc, s)  ->   (* library-level error with location info *)
        `error (strerr loc s)
    | C.Piqi_error s ->  (* library-level error *)
        `error s
    | Piqi_piqirun.Error (_pos, s) ->  (* error while parsing protobuf *)
        `rpc_error (`invalid_input ("error while parsing protobuf: " ^ s))
    | Piqi_convert.EOF ->  (* there was no input data *)
        `rpc_error `missing_input


let convert args =
  with_handle_piqi_errors convert args


let add_one_piqi input_format data =
  let piqtype = Piqi_convert.find_type "piqi" in
  match Piqi_convert.parse_obj piqtype input_format data with
    | Piqi_convert.Piqi piqi ->
        (* cache Piqi spec and preserve its location info *)
        Piqi_db.add_piqi piqi;
        Piqloc.preserve ();
        ()
    | _ -> assert false


(* "add-piqi" call handler *)
let add_piqi args =
  let open I.Add_piqi_input in (
    List.iter (add_one_piqi args.format) args.data;
    `ok_empty
  )


let add_piqi args =
  with_handle_piqi_errors add_piqi args


exception Break of Piqi_rpc_piqi.response


let do_args f data =
  let data =
    match data with
      | Some x -> x
      | None ->
          let response = return_rpc_error `missing_input in
          raise (Break response)
  in
  let buf = Piqirun.init_from_string data in
  try f buf
  with exn ->
    let response = return_rpc_error (`invalid_input (string_of_exn exn)) in
    raise (Break response)


let do_run name f x =
  trace "running function: %s\n" name;
  try f x
  with exn ->
    let response = return_rpc_error
      (`internal_error ("error while running function " ^ U.quote name ^ ": " ^ string_of_exn exn))
    in
    raise (Break response)


let execute_request req =
  let open Piqi_rpc_piqi.Request in
  match req.name, req.data with
    | "convert", data -> (
        let args = do_args I.parse_convert_input data in
        match do_run req.name convert args with
          | `ok res -> return_ok (I.gen_convert_output res)
          | `error err -> return_error (I.gen_convert_error err)
          | `rpc_error err -> return_rpc_error err
        )
    | "add-piqi", data -> (
        let args = do_args I.parse_add_piqi_input data in
        match do_run req.name add_piqi args with
          | `ok_empty -> return_ok_empty ()
          | `error err -> return_error (I.gen_add_piqi_error err)
          | `rpc_error err -> return_rpc_error err
        )
    | "ping", None ->
        return_ok_empty ()
    | "", None ->
        (* return the Piqi module and all the dependencies encoded as a list of
         * Piqi each encoded using Protobuf binary format *)
        let output = Piqirun.gen_list Piqirun.gen_string_field (-1) [I.piqi] in
        return_ok output
    | "", Some _ ->
        return_rpc_error
          (`invalid_input "no input is expected for 'get_piqi' command")
    | name, _ ->
        return_rpc_error `unknown_function


let do_work () =
  let request, caller_ref =
    try
      Piqi_rpc.receive_request stdin
    with exn -> (
      let response = return_rpc_error
        (`protocol_error
          ("error while reading command: " ^ Printexc.to_string exn))
      in
      Piqi_rpc.send_response stdout response;
      exit 1
    )
  in
  Piqi.debug_loc "do_work(0)";
  let response =
    try execute_request request
    with Break x -> x
  in
  Piqi.debug_loc "do_work(1)";
  Piqi_rpc.send_response stdout response ~caller_ref


(* read one request from stdin, execute the request, and write the response to
 * stdout *)
let main_loop () =
  while true
  do
    Piqi.debug_loc "main_loop(0)";
    do_work ();
    Piqi.debug_loc "main_loop(1)";

    (* Run a garbage collection cycle, as we don't need any created memory
     * objects any more. A short garbage collection cycle helps to improve
     * overall througput and latency.
     *
     * NOTE: Use of Gc.major_slice instead of Gc.minor showed better results.
     *)
    ignore (Gc.major_slice 0);
  done


let start_server () =
  Piqi_convert.init ();
  (* exit on SIGPIPE without printing a message about uncaught exception *)
  if Sys.os_type <> "Win32"
  then Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ ->
    (* have to close all channels explicilty to prevent getting an uncaught
     * sigpipe during execution of at_exit *)
    close_in_noerr stdin;
    close_out_noerr stdout;
    close_out_noerr stderr;
    exit 0
  ));
  main_loop ()


let set_gc_options () =
  (* Don't set custom options if the OCAMLRUNPARAM environment variable is
   * defined *)
  try ignore (Sys.getenv "OCAMLRUNPARAM")
  with Not_found ->
    let opt = Gc.get () in
    opt.Gc.minor_heap_size <- 4 * 1024 * 1024; (* Minor heap size: 4m *)
    opt.Gc.space_overhead <- 20;
    Gc.set opt


let usage = "Usage: piqi server [options]\nOptions:"


let speclist = Main.common_speclist @
  [
    (* XXX: disable warnings by default in order to prevent printing them on
     * stderr? *)
  ]

let run () =
  Main.parse_args () ~speclist ~usage ~min_arg_count:0 ~max_arg_count:0;

  (* reset Piqi module lookup paths in order to prevent them from getting loaded
   * from the filesystem; now, they can only be loaded using "add_piqi" RPC call
   *)
  Config.reset_paths ();

  (* Configure OCaml Garbage collector *)
  set_gc_options ();

  start_server ()

 
let _ =
  Main.register_command run "server"
    "Piqi-tools RPC-server -- reads commands from stdin and writes results to stdout"

