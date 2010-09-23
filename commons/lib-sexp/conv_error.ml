(* File: conv_error.ml

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

(* Conv_error: Module for Handling Errors during Automated S-expression
   Conversions *)

open Printf
open Conv

(* Errors concerning tuples *)

let tuple_of_size_n_expected loc n sexp =
  of_sexp_error (sprintf "%s_of_sexp: tuple of size %d expected" loc n) sexp


(* Errors concerning sum types *)

let stag_no_args loc sexp =
  of_sexp_error (loc ^ "_of_sexp: sum tag does not take arguments") sexp

let stag_incorrect_n_args loc tag sexp =
  let msg =
    sprintf "%s_of_sexp: sum tag %S has incorrect number of arguments" loc tag
  in
  of_sexp_error msg sexp

let stag_takes_args loc sexp =
  of_sexp_error (loc ^ "_of_sexp: sum tag must be a structured value") sexp

let nested_list_invalid_sum loc sexp =
  of_sexp_error (loc ^ "_of_sexp: a nested list is an invalid sum") sexp

let empty_list_invalid_sum loc sexp =
  of_sexp_error (loc ^ "_of_sexp: the empty list is an invalid sum") sexp

let unexpected_stag loc sexp =
  of_sexp_error (loc ^ "_of_sexp: unexpected sum tag") sexp


(* Errors concerning records *)

let record_only_pairs_expected loc sexp =
  let msg =
    loc ^
      "_of_sexp: record conversion: only pairs expected, \
      their first element must be an atom" in
  of_sexp_error msg sexp

let record_superfluous_fields ~what ~loc rev_fld_names sexp =
  let fld_names_str = String.concat " " (List.rev rev_fld_names) in
  let msg = sprintf "%s_of_sexp: %s: %s" loc what fld_names_str in
  of_sexp_error msg sexp

let record_duplicate_fields loc rev_fld_names sexp =
  record_superfluous_fields ~what:"duplicate fields" ~loc rev_fld_names sexp

let record_extra_fields loc rev_fld_names sexp =
  record_superfluous_fields ~what:"extra fields" ~loc rev_fld_names sexp

let rec record_get_undefined_loop fields = function
  | [] -> String.concat " " (List.rev fields)
  | (true, field) :: rest -> record_get_undefined_loop (field :: fields) rest
  | _ :: rest -> record_get_undefined_loop fields rest

let record_undefined_elements loc sexp lst =
  let undefined = record_get_undefined_loop [] lst in
  let msg =
    sprintf "%s_of_sexp: the following record elements were undefined: %s"
      loc undefined
  in
  of_sexp_error msg sexp

let record_list_instead_atom loc sexp =
  let msg = loc ^ "_of_sexp: list instead of atom for record expected" in
  of_sexp_error msg sexp

let record_poly_field_value loc sexp =
  let msg =
    loc ^
    "_of_sexp: cannot convert values of types resulting from polymorphic \
    record fields"
  in
  of_sexp_error msg sexp


(* Errors concerning polymorphic variants *)

exception No_variant_match of string * Sexp.t

let no_variant_match loc sexp =
  raise (No_variant_match (loc ^ "_of_sexp", sexp))

let ptag_no_args loc sexp =
  of_sexp_error (
    loc ^ "_of_sexp: polymorphic variant does not take arguments") sexp

let ptag_incorrect_n_args loc cnstr sexp =
  let msg =
    sprintf
      "%s_of_sexp: polymorphic variant tag %S has incorrect number of arguments"
      loc cnstr
  in
  of_sexp_error msg sexp

let nested_list_invalid_poly_var loc sexp =
  of_sexp_error (
    loc ^ "_of_sexp: a nested list is an invalid polymorphic variant") sexp

let empty_list_invalid_poly_var loc sexp =
  of_sexp_error (
    loc ^ "_of_sexp: the empty list is an invalid polymorphic variant") sexp

let silly_type loc sexp =
  of_sexp_error (loc ^ "_of_sexp: trying to convert a silly type") sexp

let empty_type loc sexp =
  of_sexp_error (loc ^ "_of_sexp: trying to convert an empty type") sexp
