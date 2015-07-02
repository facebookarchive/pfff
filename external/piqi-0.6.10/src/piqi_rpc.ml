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


(* This module implements common Piqi-RPC functions for sending and receiving
 * binary RPC packets over buffered channels.
 *
 * These functions are used from piqi_server.ml and piqi_call.ml
 *)


module Piqirun = Piqi_piqirun


(* write 16-bit integer in big-endian format to a channel *)
let write_int16_be ch i =
  assert (i >= 0 && i <= 0xffff);
  output_byte ch (i lsr 8);
  output_byte ch (i land 0xff)


(* write 32-bit integer in big-endian format to a channel *)
let write_int32_be ch i =
  assert (i >= 0);
  write_int16_be ch (i lsr 16);
  write_int16_be ch (i land 0xffff)


let read_int16_be ch =
  let i1 = input_byte ch in
  let i2 = input_byte ch in
  (i1 lsl 8) lor i2


let read_int32_be ch =
  let i1 = read_int16_be ch in
  let i2 = read_int16_be ch in
  (i1 lsl 16) lor i2


let read_string ch size =
  let res = String.create size in
  really_input ch res 0 size;
  res


(* serialize and send one length-delimited packet to the output channel *)
let send_packet ch caller_ref payload =
  let caller_ref_size = String.length caller_ref in
  let payload_size = Piqirun.OBuf.size payload in
  (* 2 extra bytes are needed to encode cref_size *)
  let total_size = (2 + caller_ref_size) + payload_size in

  (* write the packet:
   *    total size (32-bit)
   *    caller ref size (16-bit)
   *    caller ref
   *    payload
   *)
  write_int32_be ch total_size;
  write_int16_be ch caller_ref_size;
  output_string ch caller_ref;
  Piqirun.to_channel ch payload;

  flush ch


(* receive one length-delimited packet from the input channel *)
let receive_packet ch =
  let total_size = read_int32_be ch in
  let caller_ref_size = read_int16_be ch in
  let payload_size = total_size - (2 + caller_ref_size) in

  let caller_ref = read_string ch caller_ref_size in
  let payload = read_string ch payload_size in
  payload, caller_ref


(* encode and write request/response structure to the output channel *)
let send_request ch ?(caller_ref="") request =
  let request = Piqi_rpc_piqi.gen_request request in
  send_packet ch caller_ref request


let send_response ch ?(caller_ref="") response =
  let response = Piqi_rpc_piqi.gen_response response in
  send_packet ch caller_ref response


(* read and decode one request/response structure from the input channel *)
let receive_request ch =
  let payload, caller_ref = receive_packet ch in
  (* return the parsed request and the client reference *)
  let buf = Piqirun.init_from_string payload in
  Piqi_rpc_piqi.parse_request buf, caller_ref


let receive_response ch =
  let payload, caller_ref = receive_packet ch in
  (* return the parsed response and the client reference *)
  let buf = Piqirun.init_from_string payload in
  Piqi_rpc_piqi.parse_response buf, caller_ref

