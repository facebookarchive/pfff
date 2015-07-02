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


(* This module implements a Piqi RPC client. It reads function's arguments from
 * command-line options, sends commands via Unix pipe or HTTP to servers and
 * prints responses.
 *)


module C = Piqi_common
open C


(* command-line arguments *)
let output_encoding = Convert.output_encoding
let flag_piqi = ref false
let flag_piqi_all = ref false
let flag_piqi_light = ref false
let flag_h = ref false


(* values of HTTP headers when making Piqi-RPC requests *)
let content_type = "application/x-protobuf"
let accept = content_type
let user_agent = "Piqi/" ^ Piqi_version.version


let string_of_rpc_error = function
  | `unknown_function -> "unknown function"
  | `missing_input -> "missing input"
  | `invalid_input err -> "invalid input: " ^ err
  | `invalid_output err -> "invalid output: " ^ err
  | `internal_error err -> "internal error: " ^ err
  | `service_unavailable err -> "service unavailable: " ^ err
  | `protocol_error err -> "protocol error: " ^ err


let call_local_server ((ich, och) as _handle) func_name data =
  trace "piqi_call: calling %s\n" func_name;
  let request = Piqi_rpc_piqi.Request.({name = func_name; data = data}) in
  Piqi_rpc.send_request och request;
  let response, _caller_ref = Piqi_rpc.receive_response ich in
  match response with
    | `rpc_error err ->
        piqi_error ("remote rpc error: " ^ string_of_rpc_error err)
    | x -> x


let get_content_type status headers =
  match Piqi_http.get_header "content-type" headers with
    | None when status = 204 -> "" (* "No data" therefore no Content-Type *)
    | None ->
        piqi_error
          "HTTP rpc error: Content-Type header is missing in server's response"
    | Some x -> x


let bad_content_type ct =
  piqi_error (Printf.sprintf
    "HTTP rpc error: unexpected Content-Type for status code 200: %s" ct)


let call_http_server url body =
  trace "piqi_call: calling %s\n" url;
  let status, headers, body =
    Piqi_http.post url ~accept ~content_type ~user_agent ?body
  in
  let ct = get_content_type status headers in
  match status with
    | 200 when ct = content_type -> (* "OK" *)
        `ok body
    | 204 when ct = content_type -> (* "No Data" *)
        (* this is actually an empty return data structure, e.g. empty list *)
        `ok ""
    | 204 -> (* "No Data" *)
        `ok_empty
    | 200 ->
        bad_content_type ct
    | 500 when ct = content_type ->
        (* "Internal Server Error" with the application Content-Type header
         * means application error *)
        `error body
    | _ ->
        (* all other HTTP codes either mean `rpc_error or some other HTTP
         * transport or server-related error *)
        piqi_error (Printf.sprintf
          "HTTP rpc error: (status code = %d)\n%s" status body)


let init_piqi_common data =
  trace "piqi_call: init Piqi modules returned by the server\n";
  let buf = Piqirun.init_from_string data in
  let bin_piqi_list = Piqirun.parse_list Piqirun.parse_string_field buf in
  (* decode and load Piqi modules *)
  let piqi_list =
      List.map (fun x ->
          let buf = Piqirun.init_from_string x in
          let piqi = Piqi.piqi_of_pb buf in
          Piqi_db.add_piqi piqi;
          piqi
        ) bin_piqi_list
  in
  piqi_list


let get_local_piqi handle =
  trace "piqi_call: get Piqi\n";
  (* issue a special command to get Piqi modules from the server *) 
  let func_name = "" and data = None in
  match call_local_server handle func_name data with
    | `ok data ->
        init_piqi_common data
    | `error _ | `ok_empty ->
        piqi_error "local rpc error: invalid response to get_piqi request"
    | `rpc_error _ -> assert false (* checked earlier *)


let get_http_piqi path =
  trace "piqi_call: get Piqi from %s\n" path;
  (* issue a HTTP GET request get Piqi modules from the server *)
  let status, headers, body =
    Piqi_http.get path ~accept ~user_agent
  in
  let ct = get_content_type status headers in
  match status with
    | 200 when ct = content_type ->
        init_piqi_common body
    | 200 ->
        bad_content_type ct
    | _ ->
        piqi_error (Printf.sprintf
          "HTTP rpc error: unexpected response to get_piqi request (status code = %d)\n%s" status body)


let find_function piqi name =
  trace "piqi_call: find function %s\n" (U.quote name);
  try List.find (fun x -> x.T.Func.name = name) piqi.P.resolved_func
  with Not_found ->
    piqi_error ("server doesn't implement function: " ^ name)


let encode_input_data f args =
  trace "piqi_call: preparing input data\n";
  let t = f.T.Func.resolved_input in
  match t, args with
    | None, [] -> None
    | None, _ ->
        piqi_error "function doesn't expect input arguments"
    | Some piqtype, _ ->
        trace "piqi_call: parsing arguments\n";
        (* XXX: C.resolve_defaults := true; *)
        let piqobj = Piqi_getopt.parse_args (piqtype :> T.piqtype) args in
        let binobj = Piqobj_to_protobuf.gen_binobj piqobj in
        Some binobj


let decode_response f output =
  trace "piqi_call: decoding response\n";
  let piqobj_of_bin piqtype data =
    let buf = Piqirun.init_from_string data in
    Piqi_convert.piqobj_of_protobuf (piqtype :> T.piqtype) buf
  in
  match f.T.Func.resolved_output, output with
    | None, `ok_empty -> `ok_empty
    | Some _, `ok_empty ->
        piqi_error "unexpected empty result from server"
    | None, `ok _ ->
        piqi_error "unexpected non-empty result from server"
    | Some piqtype, `ok data ->
        let obj = piqobj_of_bin piqtype data in
        `ok obj
    | _, `error data -> (
        match f.T.Func.resolved_error with
          | None -> piqi_error "unexpected error result from server"
          | Some piqtype ->
              let obj = piqobj_of_bin piqtype data in
              `error obj
        )
    | _, `rpc_error _ -> assert false (* checked earlier *)


let with_open_pipe shell_command f =
  let handle = Unix.open_process shell_command in
  let res =
    try
      let res = f handle in
      `ok res
    with exn ->
      `error exn
  in
  let status = Unix.close_process handle in
  match status, res with
    | Unix.WEXITED 0, `ok x -> x
    | Unix.WEXITED 0, `error exn -> raise exn
    | Unix.WEXITED 127, _ ->
        piqi_error ("shell command couldn't be executed: " ^ shell_command)
    | Unix.WEXITED n, _ ->
        piqi_error ("server exited with error code " ^ (string_of_int n))
    | Unix.WSIGNALED n, _ ->
        piqi_error ("server was killed by signal " ^ (string_of_int n))
    | Unix.WSTOPPED n, _ ->
        piqi_error ("server was stopped by signal " ^ (string_of_int n))


(* NOTE: in future, we may implement a full http client that will open
 * connection once for all subsequent requests *)
let with_open_http f =
  try f ()
  with
    Piqi_http.Error s -> piqi_error ("HTTP rpc error: " ^ s)


(* returns the last element of the list *)
let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::t -> last t


let local_call (server_command, func_name) args =
  let f handle =
    (* the last Piqi module defines the interface to the server *)
    let piqi = last (get_local_piqi handle) in

    let f = find_function piqi func_name in
    let data = encode_input_data f args in

    let response = call_local_server handle func_name data in
    decode_response f response
  in
  with_open_pipe server_command f


let http_call (url, base_url, func_name) args =
  let f () =
    (* the last Piqi module defines the interface to the server *)
    let piqi = last (get_http_piqi base_url) in

    let f = find_function piqi func_name in
    let data = encode_input_data f args in

    let response = call_http_server url data in
    decode_response f response
  in
  with_open_http f


let local_get_piqi server_command =
  with_open_pipe server_command (fun handle -> get_local_piqi handle)


let http_get_piqi url =
  with_open_http (fun () -> get_http_piqi url)


let is_http_url url =
  try
    String.sub url 0 7 = "http://" ||
    String.sub url 0 8 = "https://"
  with _ -> false


let parse_url url =
  if is_http_url url
  then
    match Piqi_name.split_name url with
      | Some path, func ->
          (* remove a trailing '/' character if the url contains it *)
          let path, func =
            if func = ""
            then
              match Piqi_name.split_name path with
                | Some path, func -> path, func
                | _ -> piqi_error ("invalid HTTP URL: " ^ url)
            else path, func
          in
          if Piqi_name.is_valid_name func (* XXX: don't check? *)
          then `http (url, path, func)
          else piqi_error ("invalid function name in HTTP URL: " ^ func)
      | _ ->
          piqi_error ("invalid HTTP URL: " ^ url)
  else
    match Piqi_name.split_name url with
      | Some server, func ->
          if Piqi_name.is_valid_name func (* XXX: don't check? *)
          then `local (server, func)
          else piqi_error ("invalid function name in local URL: " ^ func)
      | _ ->
          piqi_error ("invalid local URL: " ^ url)


let call url args =
  match parse_url url with
    | `local x -> local_call x args
    | `http x -> http_call x args


let get_piqi url =
  if is_http_url url
  then http_get_piqi url
  else local_get_piqi url


let gen_result ch writer res =
  match res with
    | `ok_empty -> ()
    | `ok obj ->
        writer ch (Piqi_convert.Piqobj obj)
    | `error obj ->
        trace "piqi_call: remote function returned error:\n";
        writer stderr (Piqi_convert.Piqobj obj);
        exit 1


(*
 * Printing getopt help (piqi call <URL> -h)
 *)

open Iolist


let gen_typename x = ios "<" ^^ ios x ^^ ios ">"


let gen_piqtype t =
  gen_typename (piqi_typename t)


let max_opt_len = ref 0
let reset_padding () = max_opt_len := 0

let padded s =
  let str_len = String.length s in
  let max_len = max !max_opt_len str_len in
  max_opt_len := max_len;
  let pad_len = max_len - str_len + 4 in
  let padding = String.make pad_len ' ' in
  iol [ ios s; ios padding ]


let gen_option_help piqtype name getopt_letter getopt_doc =
  let short =
    match getopt_letter with
      | None -> ios ""
      | Some x -> ios "-" ^^ ios x ^^ ios ", "
  in
  let long = ios "--" ^^ ios name in
  let res = iol [ short; long ] in
  let res =
    match piqtype with
      | None -> res
      | Some t -> iol [ res; ios " "; gen_piqtype t ]
  in
  let res =
    match getopt_doc with
      | None -> res
      | Some x ->
          let s = Iolist.to_string res in
          iol [ padded s; ios x ]
  in
  res


let gen_default = function
  | None -> iol [] (* there is no default *)
  | Some default ->
      let ast = Piqobj.piq_of_piqi_any default in
      let str = Piq_gen.to_string ast ~nl:false in
      if String.contains str '\n' (* multiline? *)
      then
        ios " (default = ...)"
      else
        iol [ ios " (default = "; ios str; ios ")" ]


let gen_field_mode = function
    | `required -> ""
    | `optional -> " (optional)"
    | `repeated -> " (repeated)"


(* output indentation settings *)
let func_indent = ios "  "
let field_indent = ios "    "


let gen_field x =
  let open F in
  let field_mode = gen_field_mode x.mode in
  iol [
    field_indent;
    gen_option_help x.piqtype (name_of_field x) x.getopt_letter x.getopt_doc;
    ios field_mode;
    gen_default x.default;
  ]


let gen_record name x =
  let open R in
  (* first run is to calculate padding *)
  let _ =
    reset_padding ();
    List.map gen_field x.field
  in
  let fields = List.map gen_field x.field in
  if fields = []
  then iol []
  else
    iol [
      gen_typename name; ios ", which ";
      if List.tl fields = [] (* record contains only one field *)
      then ios "is:"
      else ios "is a combination of:";
      ios "\n\n";
      iod "\n" fields;
    ]


let gen_option x =
  let open O in
  iol [
    field_indent;
    gen_option_help x.piqtype (name_of_option x) x.getopt_letter x.getopt_doc;
  ]


let gen_options options =
  (* first run is to calculate padding *)
  let _ =
    reset_padding ();
    List.map gen_option options
  in
  let options = List.map gen_option options in
  iol [
    ios "\n\n";
    iod "\n" options;
  ]


let gen_variant name x =
  let open V in
  iol [
    gen_typename name;
    ios ", which is one of:";
    gen_options x.option;
  ]


let gen_enum name x =
  let open E in
  iol [
    gen_typename name;
    ios ", which is one of:";
    gen_options x.option;
  ]


let gen_list x =
  let open L in
  let typename = gen_piqtype (some_of x.piqtype) in
  iol [
    ios "["; typename; ios " ...]";
    match unalias (some_of x.piqtype) with
      | `variant x ->
          iol [
            ios ", where "; typename; ios " is one of:";
            gen_options x.V.option;
          ]
      | `enum x ->
          iol [
            ios ", where "; typename; ios " is one of:";
            gen_options x.E.option;
          ]
      | _ ->
          iol []
  ]


let gen_def name t =
  match t with
    | `record t -> gen_record name t
    | `variant t -> gen_variant name t
    | `enum t -> gen_enum name t
    | `list t -> gen_list t
    | `alias t -> assert false
    | t -> gen_typename name (* primitive type *)


let gen_input def =
  match def with
    | `alias x ->
        let t = some_of x.A.piqtype in
        let name = piqi_typename t in
        gen_def name (unalias t)
    | _ -> gen_def "input" def


let gen_func_help f =
  let open T.Func in
  let help =
    match f.resolved_input with
      | None -> ios f.name
      | Some t ->
          let params = gen_input t in
          iol [ ios f.name; ios " -- "; params ]
  in
  iol [
    func_indent;
    help;
    ios "\n\n"
  ]


let print_help ch piqi =
  let open P in

  (* first display functions that don't have input *)
  let func_list = piqi.resolved_func in
  let l1, l2 = List.partition (fun x -> x.T.Func.input = None) func_list in
  let func_list = l1 @ l2 in

  let func_help = List.map gen_func_help func_list in
  let code =
    match func_help with
      | [] ->
          ios "\nPiqi-RPC service doesn't define any functions.\n\n"
      | _ ->
          iol [
            ios "\nPiqi-RPC functions (use -p flag for more details):\n\n";
            iol func_help;
          ]
  in
  Iolist.to_channel ch code

(* -- end printing getopt help *)


let run_call url =
  let ch = Main.open_output !Main.ofile in
  if not (!flag_piqi || !flag_piqi_all || !flag_piqi_light || !flag_h)
  then
    let args = Piqi_getopt.getopt_piq () in
    let writer = Convert.make_writer !output_encoding in
    let res = call url args in
    gen_result ch writer res
  else
    let is_piqi_input = true in
    let writer = Convert.make_writer !output_encoding ~is_piqi_input in
    let piqi_list = get_piqi url in
    if !flag_piqi
    then
      let piqi = last piqi_list in
      if !output_encoding = ""
      then
        Piqi_pp.prettyprint_piqi ch (Piqi_convert.original_piqi piqi)
      else
        writer ch (Piqi_convert.Piqi piqi)
    else if !flag_piqi_all
    then
      List.iter (fun piqi -> writer ch (Piqi_convert.Piqi piqi)) piqi_list
    else if !flag_piqi_light
    then
      let piqi = last piqi_list in
      Piqi_light.gen_piqi ch piqi
    else if !flag_h
    then
      print_help ch (last piqi_list)
    else assert(false)


let usage = "Usage: piqi call [options] <URL> -- [call arguments]\nOptions:"


(* URL: <server>/<method> *)
let url = ref ""
let anon_fun s = url := s


let speclist = Main.common_speclist @
  [
    Main.arg__strict;
    Main.arg_o;

    Convert.arg_t;

    "--piqi", Arg.Set flag_piqi,
    "instead of calling a function, only print the Piqi module that defines the service";

    "--piqi-all", Arg.Set flag_piqi_all,
    "similar to --piqi, but print all Piqi modules (i.e. dependencies)";

    "--piqi-light", Arg.Set flag_piqi_light,
    "similar to --piqi, but print the Piqi modules in Piqi-light syntax";

    "-p", Arg.Set flag_piqi_light,
    "the same as --piqi-light";

    "-h", Arg.Set flag_h,
    "print command-line usage help for Piqi functions";

    Piqi_getopt.arg__rest;
  ]


let run () =
  Main.parse_args ()
    ~speclist ~usage ~min_arg_count:1 ~max_arg_count:1 ~anon_fun;

  Piqi_convert.init ();
  Piqi_getopt.init ();

  (* reset Piqi module lookup paths in order to prevent them from getting loaded
   * from the filesystem; now, they can only be loaded by getting them from
   * RPC-server using "get_piqi" RPC protocol request
   *)
  Config.reset_paths ();

  run_call !url

 
let _ =
  Main.register_command run "call"
    "Piqi-RPC client -- call remote function with command-line arguments as input"

