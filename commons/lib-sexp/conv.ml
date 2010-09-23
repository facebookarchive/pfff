(* File: conv.ml

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

(* Conv: Utility Module for S-expression Conversions *)

open Printf
open Bigarray
open Sexp

type 'a sexp_option = 'a option

(* Conversion of OCaml-values to S-expressions *)

let default_string_of_float = ref (fun n -> sprintf "%.20G" n)
let read_old_option_format = ref true
let write_old_option_format = ref true

let sexp_of_unit () = List []
let sexp_of_bool b = Atom (string_of_bool b)
let sexp_of_string str = Atom str
let sexp_of_char c = Atom (String.make 1 c)
let sexp_of_int n = Atom (string_of_int n)
let sexp_of_float n = Atom (!default_string_of_float n)
let sexp_of_int32 n = Atom (Int32.to_string n)
let sexp_of_int64 n = Atom (Int64.to_string n)
let sexp_of_nativeint n = Atom (Nativeint.to_string n)
let sexp_of_big_int n = Atom (Big_int.string_of_big_int n)
let sexp_of_nat n = Atom (Nat.string_of_nat n)
let sexp_of_num n = Atom (Num.string_of_num n)
let sexp_of_ratio n = Atom (Ratio.string_of_ratio n)
let sexp_of_ref sexp_of__a rf = sexp_of__a !rf
let sexp_of_lazy sexp_of__a lv = sexp_of__a (Lazy.force lv)

let sexp_of_option sexp_of__a = function
  | Some x when !write_old_option_format -> List [sexp_of__a x]
  | Some x -> List [Atom "some"; sexp_of__a x]
  | None when !write_old_option_format -> List []
  | None -> Atom "none"

let sexp_of_pair sexp_of__a sexp_of__b (a, b) =
  List [sexp_of__a a; sexp_of__b b]

let sexp_of_triple sexp_of__a sexp_of__b sexp_of__c (a, b, c) =
  List [sexp_of__a a; sexp_of__b b; sexp_of__c c]

let sexp_of_list sexp_of__a lst =
  List (List.rev (List.rev_map sexp_of__a lst))

let sexp_of_array sexp_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.(i) :: !lst_ref
  done;
  List !lst_ref

let sexp_of_hashtbl sexp_of_key sexp_of_val htbl =
  let coll k v acc = List [sexp_of_key k; sexp_of_val v] :: acc in
  List (Hashtbl.fold coll htbl [])

let sexp_of_float_vec vec =
  let lst_ref = ref [] in
  for i = Array1.dim vec downto 1 do
    lst_ref := sexp_of_float vec.{i} :: !lst_ref
  done;
  List !lst_ref

type vec32 = (float, float32_elt, fortran_layout) Array1.t
type vec64 = (float, float64_elt, fortran_layout) Array1.t
let sexp_of_float32_vec (vec : vec32) = sexp_of_float_vec vec
let sexp_of_float64_vec (vec : vec64) = sexp_of_float_vec vec
let sexp_of_vec (vec : vec64) = sexp_of_float_vec vec

let sexp_of_float_mat mat =
  let m = Array2.dim1 mat in
  let n = Array2.dim2 mat in
  let lst_ref = ref [] in
  for col = n downto 1 do
    let vec = Array2.slice_right mat col in
    for row = m downto 1 do
      lst_ref := sexp_of_float vec.{row} :: !lst_ref
    done
  done;
  List (sexp_of_int m :: sexp_of_int n :: !lst_ref)

type mat32 = (float, float32_elt, fortran_layout) Array2.t
type mat64 = (float, float64_elt, fortran_layout) Array2.t
let sexp_of_float32_mat (mat : mat32) = sexp_of_float_mat mat
let sexp_of_float64_mat (mat : mat64) = sexp_of_float_mat mat
let sexp_of_mat (mat : mat64) = sexp_of_float_mat mat

let sexp_of_abstr _ = Atom "<abstr>"
let sexp_of_fun _ = Atom "<fun>"

type 'a opaque = 'a
let sexp_of_opaque _ _ = Atom "<opaque>"

let string_of__of__sexp_of to_sexp x = Sexp.to_string (to_sexp x)


(* Conversion of S-expressions to OCaml-values *)

exception Of_sexp_error of string * Sexp.t

let record_check_extra_fields = ref true

let of_sexp_error what sexp = raise (Of_sexp_error (what, sexp))

let unit_of_sexp sexp = match sexp with
  | List [] -> ()
  | Atom _ | List _ -> of_sexp_error "unit_of_sexp: empty list needed" sexp

let bool_of_sexp sexp = match sexp with
  | Atom ("true" | "True") -> true
  | Atom ("false" | "False") -> false
  | Atom _ -> of_sexp_error "bool_of_sexp: unknown string" sexp
  | List _ -> of_sexp_error "bool_of_sexp: atom needed" sexp

let string_of_sexp sexp = match sexp with
  | Atom str -> str
  | List _ -> of_sexp_error "string_of_sexp: atom needed" sexp

let char_of_sexp sexp = match sexp with
  | Atom str ->
      if String.length str <> 1 then
        of_sexp_error
          "char_of_sexp: atom string must contain one character only" sexp;
      str.[0]
  | List _ -> of_sexp_error "char_of_sexp: atom needed" sexp

let int_of_sexp sexp = match sexp with
  | Atom str ->
      (try int_of_string str
      with exc -> of_sexp_error ("int_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "int_of_sexp: atom needed" sexp

let float_of_sexp sexp = match sexp with
  | Atom str ->
      (try float_of_string str
      with exc ->
        of_sexp_error ("float_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "float_of_sexp: atom needed" sexp

let int32_of_sexp sexp = match sexp with
  | Atom str ->
      (try Int32.of_string str
      with exc ->
        of_sexp_error ("int32_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "int32_of_sexp: atom needed" sexp

let int64_of_sexp sexp = match sexp with
  | Atom str ->
      (try Int64.of_string str
      with exc ->
        of_sexp_error ("int64_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "int64_of_sexp: atom needed" sexp

let nativeint_of_sexp sexp = match sexp with
  | Atom str ->
      (try Nativeint.of_string str
      with exc ->
        of_sexp_error ("nativeint_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "nativeint_of_sexp: atom needed" sexp

let big_int_of_sexp sexp = match sexp with
  | Atom str ->
      (try Big_int.big_int_of_string str
      with exc ->
        of_sexp_error ("big_int_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "big_int_of_sexp: atom needed" sexp

let nat_of_sexp sexp = match sexp with
  | Atom str ->
      (try Nat.nat_of_string str
      with exc ->
        of_sexp_error ("nat_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "nat_of_sexp: atom needed" sexp

let num_of_sexp sexp = match sexp with
  | Atom str ->
      (try Num.num_of_string str
      with exc ->
        of_sexp_error ("num_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "num_of_sexp: atom needed" sexp

let ratio_of_sexp sexp = match sexp with
  | Atom str ->
      (try Ratio.ratio_of_string str
      with exc ->
        of_sexp_error ("ratio_of_sexp: " ^ Printexc.to_string exc) sexp)
  | List _ -> of_sexp_error "ratio_of_sexp: atom needed" sexp

let ref_of_sexp a__of_sexp sexp = ref (a__of_sexp sexp)
let lazy_of_sexp a__of_sexp sexp = lazy (a__of_sexp sexp)

let option_of_sexp a__of_sexp sexp =
  if !read_old_option_format then
    match sexp with
    | List [] | Atom ("none" | "None") -> None
    | List [el] | List [Atom ("some" | "Some"); el] -> Some (a__of_sexp el)
    | List _ ->
        of_sexp_error "option_of_sexp: list must represent optional value" sexp
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
  else
    match sexp with
    | Atom ("none" | "None") -> None
    | List [Atom ("some" | "Some"); el] -> Some (a__of_sexp el)
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
    | List _ -> of_sexp_error "option_of_sexp: list must be (some el)" sexp

let pair_of_sexp a__of_sexp b__of_sexp sexp = match sexp with
  | List [a_sexp; b_sexp] ->
      let a = a__of_sexp a_sexp in
      let b = b__of_sexp b_sexp in
      a, b
  | List _ ->
      of_sexp_error
        "pair_of_sexp: list must contain exactly two elements only" sexp
  | Atom _ -> of_sexp_error "pair_of_sexp: list needed" sexp

let triple_of_sexp a__of_sexp b__of_sexp c__of_sexp sexp = match sexp with
  | List [a_sexp; b_sexp; c_sexp] ->
      let a = a__of_sexp a_sexp in
      let b = b__of_sexp b_sexp in
      let c = c__of_sexp c_sexp in
      a, b, c
  | List _ ->
      of_sexp_error
        "triple_of_sexp: list must contain exactly three elements only" sexp
  | Atom _ -> of_sexp_error "triple_of_sexp: list needed" sexp

let list_of_sexp a__of_sexp sexp = match sexp with
  | List lst ->
      let rev_lst = List.rev_map a__of_sexp lst in
      List.rev rev_lst
  | Atom _ -> of_sexp_error "list_of_sexp: list needed" sexp

let array_of_sexp a__of_sexp sexp = match sexp with
  | List [] -> [||]
  | List (h :: t) ->
      let len = List.length t + 1 in
      let res = Array.create len (a__of_sexp h) in
      let rec loop i = function
        | [] -> res
        | h :: t -> res.(i) <- a__of_sexp h; loop (i + 1) t in
      loop 1 t
  | Atom _ -> of_sexp_error "array_of_sexp: list needed" sexp

let hashtbl_of_sexp key_of_sexp val_of_sexp sexp = match sexp with
  | List lst ->
      let htbl = Hashtbl.create 0 in
      let act = function
        | List [k_sexp; v_sexp] ->
            Hashtbl.add htbl (key_of_sexp k_sexp) (val_of_sexp v_sexp)
        | List _ | Atom _ ->
            of_sexp_error "hashtbl_of_sexp: tuple list needed" sexp
      in
      List.iter act lst;
      htbl
  | Atom _ -> of_sexp_error "hashtbl_of_sexp: list needed" sexp

let float_vec_of_sexp empty_float_vec create_float_vec sexp = match sexp with
  | List [] -> empty_float_vec
  | List lst ->
      let len = List.length lst in
      let res = create_float_vec len in
      let rec loop i = function
        | [] -> res
        | h :: t -> res.{i} <- float_of_sexp h; loop (i + 1) t in
      loop 1 lst
  | Atom _ -> of_sexp_error "float_vec_of_sexp: list needed" sexp

let create_float32_vec = Array1.create float32 fortran_layout
let create_float64_vec = Array1.create float64 fortran_layout
let empty_float32_vec = create_float32_vec 0
let empty_float64_vec = create_float64_vec 0
let float32_vec_of_sexp = float_vec_of_sexp empty_float32_vec create_float32_vec
let float64_vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec
let vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec

let check_too_much_data sexp data res =
  if data = [] then res
  else of_sexp_error "float_mat_of_sexp: too much data" sexp

let float_mat_of_sexp create_float_mat sexp = match sexp with
  | List (sm :: sn :: data) ->
      let m = int_of_sexp sm in
      let n = int_of_sexp sn in
      let res = create_float_mat m n in
      if m = 0 || n = 0 then check_too_much_data sexp data res
      else
        let rec loop_cols col data =
          let vec = Array2.slice_right res col in
          let rec loop_rows row = function
            | [] -> of_sexp_error "float_mat_of_sexp: not enough data" sexp
            | h :: t ->
                vec.{row} <- float_of_sexp h;
                if row = m then
                  if col = n then check_too_much_data sexp t res
                  else loop_cols (col + 1) t
                else loop_rows (row + 1) t in
          loop_rows 1 data in
        loop_cols 1 data
  | List _ -> of_sexp_error "float_mat_of_sexp: list too short" sexp
  | Atom _ -> of_sexp_error "float_mat_of_sexp: list needed" sexp

let create_float32_mat = Array2.create float32 fortran_layout
let create_float64_mat = Array2.create float64 fortran_layout

let float32_mat_of_sexp = float_mat_of_sexp create_float32_mat
let float64_mat_of_sexp = float_mat_of_sexp create_float64_mat
let mat_of_sexp = float_mat_of_sexp create_float64_mat

let fun_of_sexp sexp =
  of_sexp_error "fun_of_sexp: cannot convert function values" sexp

let of_string__of__of_sexp of_sexp s =
  try
    let sexp = Sexp.of_string s in
    of_sexp sexp
  with e ->
    failwith (sprintf "of_string failed on %s with %s" s (Printexc.to_string e))
