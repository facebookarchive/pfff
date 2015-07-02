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

module U = Piqi_util


let has_parent name =
  String.contains name '/'


(* analogous to Filename.dirname *)
let get_module_name x =
  try
    let pos = String.rindex x '/' in
    let res = String.sub x 0 pos in
    Some res
  with
    Not_found -> None


(* analogous to Filename.basename *)
let get_local_name x =
  try
    let pos = String.rindex x '/' in
    String.sub x (pos + 1) ((String.length x) - pos - 1)
  with
    Not_found -> x


let is_scoped_name name = String.contains name '/'


let split_name x =
  get_module_name x, get_local_name x


let is_valid_char allow = function
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '-' -> true
  | x -> String.contains allow x


let is_valid_name ?(allow="") x =
  let len = String.length x in
  let rec check_char i =
    if i >= len
    then true
    else
      if is_valid_char allow x.[i] 
      then check_char (i + 1)
      else false
  in
  if len = 0 then false
  else
    match x.[0] with
      | 'a'..'z'
      | 'A'..'Z' -> check_char 1
      | _ -> false


let some f = function
  | None -> true
  | Some x -> f x
    

let is_valid_pathname x =
  (* TODO: check domain name and URL/paths validity *)
  is_valid_name x ~allow:"/._"


let is_valid_modname x =
  let dirname, basename = split_name x in
  (* NOTE: allowing underscores and dots in basename *)
  some is_valid_pathname dirname && is_valid_name basename ~allow:"_."


let is_valid_typename ?allow x =
  let modname, typename = split_name x in
  some is_valid_modname modname && is_valid_name typename ?allow


let normalize_list l =
  let isupper c = (c >= 'A' && c <= 'Z') in
  let tolower c =  Char.chr (Char.code c + 32) in
  let rec aux hump accu = function
    | [] -> List.rev accu
    | h::t when h = '_' || h = '-' ->
        aux true ('-'::accu) t
    | h::t when isupper h && not hump -> (* first hump character *)
        aux true ((tolower h)::'-'::accu) t
    | h::t when isupper h && hump -> (* another hump character *)
        aux hump ((tolower h)::accu) t
    | h::t when h = '.' || h = ':' || h = '/' ->
        aux true (h::accu) t
    | h::t -> (* end of hump *)
        aux false (h::accu) t
  in
  match l with
    | [] -> []
    | h::_ -> aux (isupper h) [] l


(* check if the name is normal, i.e. no uppercase characters and no hyphens *)
let is_normal_name s =
  let len = String.length s in
  let rec aux i =
    if i = len
    then true (* the name is normal *)
    else
      match s.[i] with
        | 'A'..'Z' | '_' -> false
        | _ -> aux (i+1)
  in
  aux 0


(* convert an arbitary valid name to lowercase name which words are separated by
 * dashes; for example "CamelCase" will become "camel-case"; already lowercased
 * names will remain intact *)
let normalize_name s =
  if is_normal_name s
  then s
  else U.string_of_list (normalize_list (U.list_of_string s))

