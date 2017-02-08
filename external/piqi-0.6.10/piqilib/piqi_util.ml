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

(*
 * commonly used utility functions
 *)

(* substitute character [x] with [y] in string [s] *)
let string_subst_char s x y =
  if not (String.contains s x)
  then s
  else
    (* preserve the original string *)
    let s = String.copy s in
    for i = 0 to (String.length s) - 1
    do
      if s.[i] = x
      then s.[i] <- y
    done; s


let list_of_string s =
  let n = String.length s in
  let rec aux i =
    if i < n
    then s.[i] :: (aux (i+1))
    else []
  in aux 0


let string_of_list l =
  let s = String.create (List.length l) in
  let rec aux i = function
    | [] -> ()
    | h::t ->
        s.[i] <- h; aux (i+1) t
  in
  aux 0 l; s


let dashes_to_underscores s =
  string_subst_char s '-' '_'


let underscores_to_dashes s =
  string_subst_char s '_' '-'


(* split string [s] into stubstring using character [sep] as a separator *) 
let string_split s ?(start=0) sep =
  let rec aux len i accu =
    if i < start
    then
      let name = String.sub s 0 (i+len+1) in
      name::accu
    else
      let c = s.[i] in
      if c = sep
      then
        let part = String.sub s (i + 1) len in
        aux 0 (i - 1) (part :: accu)
      else
        aux (len + 1) (i - 1) accu
  in
  let len = String.length s in
  if not (String.contains s sep) || len = 0
  then [s]
  else aux 0 (len - 1) []


(* run (unit -> 'a) function with some boolean reference set to the specified
 * value before than and have the value restored back to its original value
 * after that *)
let with_bool bool_ref value f =
   let saved_value = !bool_ref in
   bool_ref := value;
   try
     let res = f () in
     bool_ref := saved_value;
     res
   with exn ->
     bool_ref := saved_value;
     raise exn


(* list flatmap *)
let flatmap f l =  List.concat (List.map f l)


let find_dups l =
  let rec aux = function
    | [] -> None
    | h::t ->
        try
          let dup = List.find (fun x -> x = h) t in
          Some (dup, h)
        with Not_found -> aux t
  in aux l


let quote s = "\"" ^ s ^ "\""


(* NOTE: naive, non-tail recursive. Remove duplicates from the list using
 * reference equality, preserves the initial order *)
let rec uniqq = function
  | [] -> []
  | h::t ->
      let t = uniqq t in
      if List.memq h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniqq l =
  List.rev (uniqq (List.rev l))


let rec uniq = function
  | [] -> []
  | h::t ->
      let t = uniq t in
      if List.mem h t then t else h :: t


(* leave the first of the duplicate elements in the list instead of the last *)
let uniq l =
  List.rev (uniq (List.rev l))

