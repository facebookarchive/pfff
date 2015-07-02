(*
   Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik

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


(* auxiliary iolist type and related primitives *)
type iolist =
    Ios of string
  | Iol of iolist list
  | Iob of char
  | Indent | Unindent | Eol


(* iolist construction *)
let (^^) a b =
  match a, b with
    | Ios _, Iol b -> Iol (a::b)
    | Ios " ", Eol -> Eol
    | Ios " ", Indent -> Indent
    | _, _ -> Iol [a;b]


let eol = Eol
let indent = Iol [Indent; eol]
let unindent = Unindent


let ios x = Ios x
let iol l = Iol l
let iob b = Iob b
let iod delim = function  (* iol with elements separated by delim *)
  | [] -> Iol []
  | h::t ->
    let d = ios delim in
    List.fold_left (fun accu x -> accu ^^ d ^^ x) h t
let ioq x = (* double-quoted string *)
  iol [ios "\""; ios x; ios "\""]


(* iolist output *)
let to_buffer0 buf l =
  let indent = ref 0 in
  let rec aux = function
    | Eol | Ios "\n" | Iob '\n' ->
        Buffer.add_char buf '\n';
        for i = 1 to !indent
        do Buffer.add_string buf "    "
        done
    | Ios s -> Buffer.add_string buf s
    | Iol l -> List.iter aux l
    | Iob b -> Buffer.add_char buf b
    | Indent -> incr indent
    | Unindent -> decr indent; if !indent < 0 then indent := 0
  in aux l


(* iolist output size for binary output mode only -- no indentation handling *)
let size l =
  let rec aux = function
    | Ios s -> String.length s
    | Iol l -> List.fold_left (fun accu x -> accu + (aux x)) 0 l
    | Iob _ -> 1
    | _ -> assert false
  in aux l


let to_string l =
  let buf = Buffer.create (size l) in
  to_buffer0 buf l;
  Buffer.contents buf


let to_buffer l =
  let buf = Buffer.create 80 in
  to_buffer0 buf l;
  buf


let to_channel ch code =
  let buf = to_buffer code in
  Buffer.output_buffer ch buf

