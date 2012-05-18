(* File: path.ml

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http://www.janestcapital.com/ocaml

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* Path: Module for Substitutions within S-expressions *)

open Format

open Sexp

type el = Pos of int | Match of string * int | Rec of string
type t = el list

let illegal_atom loc sexp =
  failwith (sprintf "Path.%s: illegal atom: %s" loc (Sexp.to_string sexp))

let extract_pos_lst loc sexp ix lst =
  let rec loop acc n = function
    | [] ->
        let sexp_str = Sexp.to_string sexp in
        failwith (
          sprintf "Path.%s: illegal index %d in: %s" loc ix sexp_str)
    | h :: t ->
        if n = 0 then
          let subst = function
            | None -> List.rev_append acc t
            | Some x -> List.rev_append acc (x :: t) in
          subst, h
        else loop (h :: acc) (n - 1) t in
  loop [] ix lst

let extract_pos n = function
  | List lst as sexp ->
      let subst, el = extract_pos_lst "extract_pos" sexp n lst in
      (fun x -> List (subst x)), el
  | Atom _ as sexp -> illegal_atom "extract_pos" sexp

let extract_match tag arg_ix = function
  | List (Atom str as sexp :: args) when str = tag ->
      let subst, el = extract_pos_lst "extract_match" (List args) arg_ix args in
      (fun maybe_x -> List (sexp :: subst maybe_x)), el
  | List _ as sexp ->
      let sexp_str = Sexp.to_string sexp in
      failwith ("Path.extract_match: unexpected nested list in: " ^ sexp_str)
  | Atom _ as sexp -> illegal_atom "extract_match" sexp

let extract_rec key = function
  | List lst as sexp ->
      let rec loop acc = function
        | [] ->
            let sexp_str = Sexp.to_string sexp in
            failwith (
              sprintf "Path.extract_rec: key \"%s\" not found in: %s"
                key sexp_str)
        | List [Atom str as sexp; v] :: rest when str = key ->
            let subst x = List (List.rev_append acc (List [sexp; x] :: rest)) in
            subst, v
        | h :: t -> loop (h :: acc) t in
      loop [] lst
  | Atom _ as sexp -> illegal_atom "extract_rec" sexp

let id x = x

let rec subst_option (sup_subst, el) rest =
  let sub_subst, sub_el = subst_path el rest in
  let subst x = sup_subst (Some (sub_subst x)) in
  subst, sub_el

and subst_path sexp = function
  | Pos n :: t -> subst_option (extract_pos n sexp) t
  | Match (tag, arg_ix) :: t -> subst_option (extract_match tag arg_ix sexp) t
  | Rec key :: rest ->
      let rec_subst, el = extract_rec key sexp in
      let sub_subst, sub_el = subst_path el rest in
      let subst x = rec_subst (sub_subst x) in
      subst, sub_el
  | [] -> id, sexp

let implode lst =
  let len = List.length lst in
  let str = String.create len in
  let rec loop ix = function
    | h :: t -> str.[ix] <- h; loop (ix + 1) t
    | [] -> str in
  loop 0 lst

let fail_parse msg = failwith ("Path.parse: " ^ msg)

let parse str =
  let len = String.length str in
  if len = 0 then fail_parse "path empty"
  else
    let rec loop acc dot_ix =
      match str.[dot_ix] with
      | '.' ->
          let dot_ix1 = dot_ix + 1 in
          if dot_ix1 = len then List.rev acc
          else
            let rec parse_dot acc str_acc ix =
              if ix = len then
                List.rev_append acc [Rec (implode (List.rev str_acc))]
              else
                match str.[ix] with
                | '[' ->
                    let rec parse_index index_acc ix =
                      if ix = len then fail_parse "EOF reading index"
                      else
                        match str.[ix], index_acc with
                        | '0'..'9' as c, None ->
                            parse_index (Some (int_of_char c - 48)) (ix + 1)
                        | '0'..'9' as c, Some index_acc ->
                            let new_index_acc =
                              Some (10 * index_acc + int_of_char c - 48) in
                            parse_index new_index_acc (ix + 1)
                        | ']', None -> fail_parse "empty index"
                        | ']', Some index_acc ->
                            let path_el =
                              if str_acc = [] then Pos index_acc
                              else
                                Match (implode (List.rev str_acc), index_acc) in
                            let ix1 = ix + 1 in
                            if ix1 = len then List.rev_append acc [path_el]
                            else loop (path_el :: acc) ix1
                        | c, _ ->
                            fail_parse (
                              sprintf "illegal character in index: %c" c) in
                    parse_index None (ix + 1)
                | '\\' ->
                    let ix1 = ix + 1 in
                    if ix1 = len then fail_parse "EOF after escape"
                    else parse_dot acc (str.[ix1] :: str_acc) (ix + 1)
                | '.' ->
                    if str_acc = [] then fail_parse "double '.'";
                    let path_el = Rec (implode (List.rev str_acc)) in
                    parse_dot (path_el :: acc) [] (ix + 1)
                | c -> parse_dot acc (c :: str_acc) (ix + 1) in
            parse_dot acc [] dot_ix1
      | c -> fail_parse (sprintf "'.' expected; got '%c'" c) in
    loop [] 0

let get_subst path str sexp =
  let path =
    match path, str with
    | Some path, _ -> path
    | None, Some str -> parse str
    | None, None -> [] in
  subst_path sexp path

let get ?path ?str sexp = snd (get_subst path str sexp)

let replace ?path ?str sexp ~subst =
  let subst_fun, _ = get_subst path str sexp in
  subst_fun subst

let replace_no_path ~str sexp ~subst = replace ~str sexp ~subst
