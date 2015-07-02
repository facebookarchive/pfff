module Piqirun = Piqi_piqirun
  
module rec Piqi_rpc_piqi :
             sig
               type binary = string
               
               type response =
                 [
                   | `ok_empty
                   | `ok of Piqi_rpc_piqi.binary
                   | `error of Piqi_rpc_piqi.binary
                   | `rpc_error of Piqi_rpc_piqi.rpc_error
                 ]
               
               type client_error =
                 [
                   | `unknown_function
                   | `invalid_input of string
                   | `missing_input
                   | `protocol_error of string
                 ]
               
               type server_error =
                 [
                   | `invalid_output of string
                   | `internal_error of string
                   | `service_unavailable of string
                 ]
               
               type rpc_error = [ | client_error | server_error ]
               
               type request = Request.t
               
             end = Piqi_rpc_piqi
and
  Request :
    sig
      type t =
        { mutable name : string; mutable data : Piqi_rpc_piqi.binary option
        }
      
    end = Request
  
let rec parse_string x = Piqirun.string_of_block x
and parse_binary x = Piqirun.string_of_block x
and parse_request x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_required_field 1 parse_string x in
  let (_data, x) = Piqirun.parse_optional_field 2 parse_binary x
  in
    (Piqirun.check_unparsed_fields x;
     { Request.name = _name; Request.data = _data; })
and parse_response x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 when x = (Piqirun.Varint 1) -> `ok_empty
    | 2 -> let res = parse_binary x in `ok res
    | 3 -> let res = parse_binary x in `error res
    | 4 -> let res = parse_rpc_error x in `rpc_error res
    | _ -> Piqirun.error_variant x code
and parse_client_error x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 404 when x = (Piqirun.Varint 1) -> `unknown_function
    | 400 -> let res = parse_string x in `invalid_input res
    | 411 when x = (Piqirun.Varint 1) -> `missing_input
    | 1 -> let res = parse_string x in `protocol_error res
    | _ -> Piqirun.error_variant x code
and parse_server_error x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 502 -> let res = parse_string x in `invalid_output res
    | 500 -> let res = parse_string x in `internal_error res
    | 503 -> let res = parse_string x in `service_unavailable res
    | _ -> Piqirun.error_variant x code
and parse_rpc_error x =
  let (code, x) = Piqirun.parse_variant x
  in
    match code with
    | 1 -> (parse_client_error x :> Piqi_rpc_piqi.rpc_error)
    | 2 -> (parse_server_error x :> Piqi_rpc_piqi.rpc_error)
    | _ -> Piqirun.error_variant x code
  
let rec gen__string code x = Piqirun.string_to_block code x
and gen__binary code x = Piqirun.string_to_block code x
and gen__request code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Request.name in
  let _data = Piqirun.gen_optional_field 2 gen__binary x.Request.data
  in Piqirun.gen_record code [ _name; _data ]
and gen__response code (x : Piqi_rpc_piqi.response) =
  Piqirun.gen_record code
    [ (match x with
       | `ok_empty -> Piqirun.gen_bool_field 1 true
       | `ok x -> gen__binary 2 x
       | `error x -> gen__binary 3 x
       | `rpc_error x -> gen__rpc_error 4 x) ]
and gen__client_error code (x : Piqi_rpc_piqi.client_error) =
  Piqirun.gen_record code
    [ (match x with
       | `unknown_function -> Piqirun.gen_bool_field 404 true
       | `invalid_input x -> gen__string 400 x
       | `missing_input -> Piqirun.gen_bool_field 411 true
       | `protocol_error x -> gen__string 1 x) ]
and gen__server_error code (x : Piqi_rpc_piqi.server_error) =
  Piqirun.gen_record code
    [ (match x with
       | `invalid_output x -> gen__string 502 x
       | `internal_error x -> gen__string 500 x
       | `service_unavailable x -> gen__string 503 x) ]
and gen__rpc_error code (x : Piqi_rpc_piqi.rpc_error) =
  Piqirun.gen_record code
    [ (match x with
       | (#Piqi_rpc_piqi.client_error as x) -> gen__client_error 1 x
       | (#Piqi_rpc_piqi.server_error as x) -> gen__server_error 2 x) ]
  
let gen_string x = gen__string (-1) x
  
let gen_binary x = gen__binary (-1) x
  
let gen_request x = gen__request (-1) x
  
let gen_response x = gen__response (-1) x
  
let gen_client_error x = gen__client_error (-1) x
  
let gen_server_error x = gen__server_error (-1) x
  
let gen_rpc_error x = gen__rpc_error (-1) x
  
let rec default_string () = ""
and default_binary () = ""
and default_request () =
  { Request.name = default_string (); Request.data = None; }
and default_response () = `ok_empty
and default_client_error () = `unknown_function
and default_server_error () = `invalid_output (default_string ())
and default_rpc_error () =
  (default_client_error () :> Piqi_rpc_piqi.rpc_error)
  
include Piqi_rpc_piqi
  

