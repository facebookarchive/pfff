(* 
 * Yoann Padioleau 
 *
 * Copyright (C) 2009-2012 Facebook
 * 
 * Most of the code in this file was inspired by code by Gazagnaire.
 * Here is the original copyright:
 * 
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Common

(*module S = Sexp*)
module J = Json_type

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * OCaml hacks to support reflection.
 * 
 * OCaml does not support reflection, and it's a good thing: we love
 * strong type-checking that forbids too clever hacks like 'eval', or
 * run-time reflection; it's too much power for you, you will misuse
 * it. At the same time it's sometimes useful. So at least we could make
 * it possible to still reflect on the type definitions or values in
 * OCaml source code. We can do it by processing ML source code and
 * emitting ML source code containing under the form of regular ML
 * value or functions meta-information about information in other
 * source code files. It's a little bit a poor's man reflection mechanism,
 * because it's more manual, but it's for the best. Metaprogramming had
 * to be painful, because it is dangerous!
 * 
 * Example: 
 *  
 *      TODO
 * 
 * In some sense we reimplement what is in the OCaml compiler, which
 * contains the full AST of OCaml source code. But the OCaml compiler
 * and its AST are too big, too scary for many tasks that would be satisfied
 * by a restricted but simpler AST.
 * 
 * Camlp4 is obviously also a solution to this problem, but it has a
 * learning curve, and it's a slightly different world than the pure
 * regular OCaml world. So this module, and ocamltarzan together can
 * reduce the problem by taking the best of camlp4, while still
 * avoiding it.
 * 
 * 
 * 
 * The support is partial. We support only the OCaml constructions
 * we found the most useful for programming stuff like
 * stub generators. 
 * 
 * less? not all OCaml so call it miniml.ml  ? or reflection.ml ?
 * 
 * 
 * Notes: 2 worlds 
 *   - the type level world,
 *   - the data level world
 * 
 * Then there is whether the code is generated on the fly, or output somewhere
 * to be compiled and linked again (so 2 steps process, more manual, but
 * arguably less complicated magic)
 * 
 * different level of (meta)programming:
 *
 *  - programming in OCaml on OCaml values (classic)
 *  - programming in OCaml on Sexp.t value of value
 *  - programming in OCaml on Sexp.t value of type description
 *  - programming in OCaml on OCaml.v value of value
 *  - programming in OCaml on OCaml.t value of type description
 * 
 * Depending on what you have to do, some levels are more suited than other.
 * For instance to do a show, to pretty print value, then sexp is good,
 * because really you just want to write code that handle 2 cases, 
 * atoms and list. That's really what pretty printing is all about. You
 * could write a pretty printer for Ocaml.v, but it will need to handle
 * 10 cases. Now if you want to write a code generator for python, or an ORM,
 * then Ocaml.v is better than sexp, because in sexp you lost some valuable
 * information (that you may have to reverse engineer, like whether 
 * a Sexp.List corresponds to a field, or a sum, or wether something is
 * null or an empty list, or wether it's an int or float, etc).
 * 
 * Another way to do (meta)programming is:
 *  - programming in Camlp4 on OCaml ast
 *  - writing camlmix code to generate code.
 * 
 * notes:
 *  - sexp value or sexp of type description, not as precise, but easier to 
 *    write really generic code that do not need to have more information
 *    about the sexp nodes (such as wether it's a field, a constuctor, etc)
 *  - miniml value or type, not as precise that the regular type,
 *    but more precise than sexp, and allow write some generic code.
 *  - ocaml value (not type as you cant program at type level),
 *    precise type checking, but can be tedious to write generic
 *    code like generic visitors or pickler/unpicklers
 * 
 * This file is working with ocamltarzan/pa/pa_type.ml (and so indirectly
 * it is working with camlp4).
 * 
 * Note that can even generate sexp_of_x for miniML :) really
 * reflexive tower here
 * 
 * Note that even if this module helps a programmer to avoid
 * using directly camlp4 to auto generate some code, it can 
 * not solve all the tasks. 
 *
 * history: 
 *  - Thought about it when wanting to do the ast_php.ml to be
 *    transformed into a .adsl declaration to be able to generate
 *    corresponding python classes using astgen.py.
 *  - Thought about a miniMLType and miniMLValue, and then realize
 *    that that was maybe what code in the ocaml-orm-sqlite
 *    was doing (type-of et value-of), except I wanted the 
 *    ocamltarzan style of meta-programming instead of the camlp4 one.
 * 
 * 
 * Alternatives:
 *  - camlp4
 *    obviously camlp4 has access to the full AST of OCaml, but 
 *    that is one pb, that's too much. We often want only to do 
 *    analysis on the type
 *  - type-conv
 *    good, but force to use camlp4. Can use the generic sexplib
 *    and then work on the generated sexp, but as explained below, 
 *    is will be on the value.
 *  - use lib-sexp (just the sexp library part, not the camlp4 support part)
 *    but not enough info. Even if usually
 *    can reverse engineer the sexp to rediscover the type,
 *    you will reverse engineer a value; what you want
 *    is the sexp representation of the type! not a value of this type.
 *    Also lib-sexp autogenerated code can be hard to understand, especially
 *    if the type definition is complex. A good side effect of ocaml.ml
 *    is that it provides an intermediate step :) So even if you 
 *    could pretty print value from your def to sexp directly, you could
 *    also use transform your value into a Ocaml.v, then use 
 *    the somehow more readable function that translate a v into a sexp,
 *    and same when wanting to read a value from a sexp, by using
 *    again Ocaml.v as an intermediate. It's nevertheless obviously
 *    less efficient.
 * 
 *  - zephyr, or thrift ?
 *  - F# ?
 *  - Lisp/Scheme ?
 *  - .Net interoperability
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* src: 
 *  - orm-sqlite/value/value.ml
 *  (itself a fork of http://xenbits.xen.org/xapi/xen-api-libs.hg?file/7a17b2ab5cfc/rpc-light/rpc.ml)
 *  - orm-sqlite/type-of/type.ml
 * 
 * update: Gazagnaire made a paper about that.
 * 
 * modifications: 
 *  - slightly renamed the types and rearrange order of constructors.  Could 
 *    have use nested modules to allow to reuse Int in different contexts,  
 *    but I actually prefer to prefix the values with the V, so when debugging
 *    stuff, it's clearer that what you are looking are values, not types
 *    (even if the ocaml toplevel would prefix the value with a V. or T.,
 *    but sexp would not)
 *  - Changed Int of int option
 *  - Introduced List, Apply, Poly
 *  - debugging support (using sexp :) )
 *)

(* OCaml type definitions *)
type t =
  | Unit 
  | Bool | Float | Char | String | Int

  | Tuple of t list
  | Dict of (string * [`RW|`RO] * t) list
  | Sum of (string * t list) list

  | Var of string
  | Poly of string
  | Arrow of t * t

  | Apply of string * t

  (* special cases of Apply *) 
  | Option of t
  | List of t 

  (* todo? split in another type, because here it's the left part, 
   * whereas before is the right part of a type definition. Also
   * have not the polymorphic args to some defs like ('a, 'b) Hashbtbl
   * | Rec of string * t 
   * | Ext of string * t
   * 
   * | Enum of t (* ??? *)
   *)

  | TTODO of string
  (* with tarzan *)

(* OCaml values (a restricted form of expressions) *)
type v = 
  | VUnit 
  | VBool of bool | VFloat of float | VInt of int (* was int64 *)
  | VChar of char | VString of string

  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list

  | VVar of (string * int64)
  | VArrow of string

  (* special cases *) 
  | VNone | VSome of v
  | VList of v list
  | VRef of v

(*
  | VEnum of v list (* ??? *)

  | VRec of (string * int64) * v
  | VExt of (string * int64) * v
*)

  | VTODO of string
  (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* the generated code can use that if he wants *)
let (_htype: (string, t) Hashtbl.t) = 
  Hashtbl.create 101
let (add_new_type: string -> t -> unit) = fun s t ->
  Hashtbl.add _htype s t
let (get_type: string -> t) = fun s ->
  Hashtbl.find _htype s
  


(* for generated code that want to transform and in and out of a v or t *)
let vof_unit () = 
  VUnit
let vof_int x = 
  VInt ((*Int64.of_int*) x)
let vof_float x = 
  VFloat ((*Int64.of_int*) x)
let vof_string x = 
  VString x
let vof_bool b = 
  VBool b
let vof_list ofa x = 
  VList (List.map ofa x)
let vof_option ofa x =
  match x with
  | None -> VNone
  | Some x -> VSome (ofa x)
let vof_ref ofa x =
  match x with
  | {contents = x } -> VRef (ofa x)
let vof_either _of_a _of_b =
  function
  | Left v1 -> let v1 = _of_a v1 in VSum (("Left", [ v1 ]))
  | Right v1 -> let v1 = _of_b v1 in VSum (("Right", [ v1 ]))

let vof_either3 _of_a _of_b _of_c =
  function
  | Left3 v1 -> let v1 = _of_a v1 in VSum (("Left3", [ v1 ]))
  | Middle3 v1 -> let v1 = _of_b v1 in VSum (("Middle3", [ v1 ]))
  | Right3 v1 -> let v1 = _of_c v1 in VSum (("Right3", [ v1 ]))






let int_ofv = function
  | VInt x -> x
  | _ -> failwith "ofv: was expecting a VInt"
let float_ofv = function
  | VFloat x -> x
  | _ -> failwith "ofv: was expecting a VFloat"
let string_ofv = function
  | VString x -> x
  | _ -> failwith "ofv: was expecting a VString"
let unit_ofv = function
  | VUnit -> ()
  | _ -> failwith "ofv: was expecting a VUnit"

let list_ofv a__of_sexp sexp = match sexp with
  | VList lst ->
      let rev_lst = List.rev_map a__of_sexp lst in
      List.rev rev_lst
  | _ -> failwith "list_ofv: VLlist needed"

let option_ofv a__of_sexp sexp = match sexp with
  | VNone -> None
  | VSome x -> Some (a__of_sexp x)
  | _ -> failwith "option_ofv: VNone or VSome needed"

type loc = string

(* todo? the function names are a direct copy of the sexp_of_v module, 
 * should probably find better names *)
let stag_incorrect_n_args loc tag v = 
  failwith ("stag_incorrect_n_args on: " ^ tag)

let unexpected_stag loc v = 
  failwith ("unexpected_stag:")

let record_only_pairs_expected loc v = 
  failwith ("record_only_pairs_expected:")

let record_duplicate_fields loc dup_flds v = 
  failwith ("record_duplicate_fields:")

let record_extra_fields loc flds v =
  failwith ("record_extra_fields:")

let record_undefined_elements loc v xs = 
  failwith ("record_undefined_elements:")

let record_list_instead_atom loc v = 
  failwith ("record_list_instead_atom:")

let tuple_of_size_n_expected  loc n v = 
  failwith (spf "tuple_of_size_n_expected: %d, got %s" n (Common2.dump v))



(*****************************************************************************)
(* Sexp converters and pretty printing *)
(*****************************************************************************)

(* pretty printing code:
 *  - using sexps :), so meta-programming on meta-programming :)
 *  - using Format, useful to copy paste and use as ML patterns
 *)

(* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_sof.cmo  pr_o.cmo /tmp/xxx.ml  *)
(*
let rec sexp_of_t =
  function
  | Unit -> Sexp.Atom "Unit"
  | Bool -> Sexp.Atom "Bool"
  | Float -> Sexp.Atom "Float"
  | Char -> Sexp.Atom "Char"
  | String -> Sexp.Atom "String"
  | Int -> Sexp.Atom "Int"
  | Tuple v1 ->
      let v1 = Conv.sexp_of_list sexp_of_t v1
      in Sexp.List [ Sexp.Atom "Tuple"; v1 ]
  | Dict v1 ->
      let v1 =
        Conv.sexp_of_list
          (fun (v1, v2, v3) ->
             let v1 = Conv.sexp_of_string v1
             and v2 =
               match v2 with | `RW -> Sexp.Atom "RW" | `RO -> Sexp.Atom "RO"
             and v3 = sexp_of_t v3
             in Sexp.List [ v1; v2; v3 ])
          v1
      in Sexp.List [ Sexp.Atom "Dict"; v1 ]
  | Sum v1 ->
      let v1 =
        Conv.sexp_of_list
          (fun (v1, v2) ->
             let v1 = Conv.sexp_of_string v1
             and v2 = Conv.sexp_of_list sexp_of_t v2
             in Sexp.List [ v1; v2 ])
          v1
      in Sexp.List [ Sexp.Atom "Sum"; v1 ]
  | Var v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "Var"; v1 ]
  | Poly v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "Poly"; v1 ]
  | Arrow ((v1, v2)) ->
      let v1 = sexp_of_t v1
      and v2 = sexp_of_t v2
      in Sexp.List [ Sexp.Atom "Arrow"; v1; v2 ]
  | Apply ((v1, v2)) ->
      let v1 = Conv.sexp_of_string v1
      and v2 = sexp_of_t v2
      in Sexp.List [ Sexp.Atom "Apply"; v1; v2 ]
  | Option v1 ->
      let v1 = sexp_of_t v1 in Sexp.List [ Sexp.Atom "Option"; v1 ]
  | List v1 -> let v1 = sexp_of_t v1 in Sexp.List [ Sexp.Atom "List"; v1 ]
  | TTODO v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "TTODO"; v1 ]
  
let rec sexp_of_v =
  function
  | VUnit -> Sexp.Atom "VUnit"
  | VBool v1 ->
      let v1 = Conv.sexp_of_bool v1 in Sexp.List [ Sexp.Atom "VBool"; v1 ]
  | VFloat v1 ->
      let v1 = Conv.sexp_of_float v1 in Sexp.List [ Sexp.Atom "VFloat"; v1 ]
  | VChar v1 ->
      let v1 = Conv.sexp_of_char v1 in Sexp.List [ Sexp.Atom "VChar"; v1 ]
  | VString v1 ->
      let v1 = Conv.sexp_of_string v1
      in Sexp.List [ Sexp.Atom "VString"; v1 ]
  | VInt v1 ->
      let v1 = Conv.sexp_of_int v1 in Sexp.List [ Sexp.Atom "VInt"; v1 ]
  | VTuple v1 ->
      let v1 = Conv.sexp_of_list sexp_of_v v1
      in Sexp.List [ Sexp.Atom "VTuple"; v1 ]
  | VDict v1 ->
      let v1 =
        Conv.sexp_of_list
          (fun (v1, v2) ->
             let v1 = Conv.sexp_of_string v1
             and v2 = sexp_of_v v2
             in Sexp.List [ v1; v2 ])
          v1
      in Sexp.List [ Sexp.Atom "VDict"; v1 ]
  | VSum ((v1, v2)) ->
      let v1 = Conv.sexp_of_string v1
      and v2 = Conv.sexp_of_list sexp_of_v v2
      in Sexp.List [ Sexp.Atom "VSum"; v1; v2 ]
  | VVar v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) ->
             let v1 = Conv.sexp_of_string v1
             and v2 = Conv.sexp_of_int64 v2
             in Sexp.List [ v1; v2 ])
      in Sexp.List [ Sexp.Atom "VVar"; v1 ]
  | VArrow v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "VArrow"; v1 ]
  | VNone -> Sexp.Atom "VNone"
  | VSome v1 -> let v1 = sexp_of_v v1 in Sexp.List [ Sexp.Atom "VSome"; v1 ]
  | VRef v1 -> let v1 = sexp_of_v v1 in Sexp.List [ Sexp.Atom "VRef"; v1 ]
  | VList v1 ->
      let v1 = Conv.sexp_of_list sexp_of_v v1
      in Sexp.List [ Sexp.Atom "VList"; v1 ]
  | VTODO v1 ->
      let v1 = Conv.sexp_of_string v1 in Sexp.List [ Sexp.Atom "VTODO"; v1 ]
  


let rec t_of_sexp__ =
  let _loc = "Xxx.t"
  in
    function
    | Sexp.Atom "Unit" -> Unit
    | Sexp.Atom "Bool" -> Bool
    | Sexp.Atom "Float" -> Float
    | Sexp.Atom "Char" -> Char
    | Sexp.Atom "String" -> String
    | Sexp.Atom "Int" -> Int
    | (Sexp.List (Sexp.Atom (("Tuple" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.list_of_sexp t_of_sexp v1 in Tuple v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Dict" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               Conv.list_of_sexp
                 (function
                  | Sexp.List ([ v1; v2; v3 ]) ->
                      let v1 = Conv.string_of_sexp v1
                      and v2 =
                        (fun sexp ->
                           try
                             match sexp with
                             | (Sexp.Atom atom as sexp) ->
                                 (match atom with
                                  | "RW" -> `RW
                                  | "RO" -> `RO
                                  | _ ->
                                      Conv_error.no_variant_match _loc sexp)
                             | (Sexp.List (Sexp.Atom atom :: _) as sexp) ->
                                 (match atom with
                                  | "RW" -> Conv_error.ptag_no_args _loc sexp
                                  | "RO" -> Conv_error.ptag_no_args _loc sexp
                                  | _ ->
                                      Conv_error.no_variant_match _loc sexp)
                             | (Sexp.List (Sexp.List _ :: _) as sexp) ->
                                 Conv_error.nested_list_invalid_poly_var _loc
                                   sexp
                             | (Sexp.List [] as sexp) ->
                                 Conv_error.empty_list_invalid_poly_var _loc
                                   sexp
                           with
                           | Conv_error.No_variant_match ((msg, sexp)) ->
                               Conv.of_sexp_error msg sexp)
                          v2
                      and v3 = t_of_sexp v3
                      in (v1, v2, v3)
                  | sexp -> Conv_error.tuple_of_size_n_expected _loc 3 sexp)
                 v1
             in Dict v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Sum" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               Conv.list_of_sexp
                 (function
                  | Sexp.List ([ v1; v2 ]) ->
                      let v1 = Conv.string_of_sexp v1
                      and v2 = Conv.list_of_sexp t_of_sexp v2
                      in (v1, v2)
                  | sexp -> Conv_error.tuple_of_size_n_expected _loc 2 sexp)
                 v1
             in Sum v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Var" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in Var v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Poly" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in Poly v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Arrow" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = t_of_sexp v1 and v2 = t_of_sexp v2 in Arrow ((v1, v2))
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Apply" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = Conv.string_of_sexp v1
             and v2 = t_of_sexp v2
             in Apply ((v1, v2))
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("Option" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = t_of_sexp v1 in Option v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("List" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = t_of_sexp v1 in List v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("TTODO" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in TTODO v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom "Unit" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.List (Sexp.Atom "Bool" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.List (Sexp.Atom "Float" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.List (Sexp.Atom "Char" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.List (Sexp.Atom "String" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.List (Sexp.Atom "Int" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.Atom "Tuple" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Dict" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Sum" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Var" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Poly" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Arrow" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Apply" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "Option" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "List" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "TTODO" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.List (Sexp.List _ :: _) as sexp) ->
        Conv_error.nested_list_invalid_sum _loc sexp
    | (Sexp.List [] as sexp) -> Conv_error.empty_list_invalid_sum _loc sexp
    | sexp -> Conv_error.unexpected_stag _loc sexp
and t_of_sexp sexp = t_of_sexp__ sexp
  
let rec v_of_sexp__ =
  let _loc = "Xxx.v"
  in
    function
    | Sexp.Atom "VUnit" -> VUnit
    | (Sexp.List (Sexp.Atom (("VBool" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.bool_of_sexp v1 in VBool v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VFloat" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.float_of_sexp v1 in VFloat v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VChar" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.char_of_sexp v1 in VChar v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VString" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in VString v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VInt" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.int_of_sexp v1 in VInt v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VTuple" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.list_of_sexp v_of_sexp v1 in VTuple v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VDict" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               Conv.list_of_sexp
                 (function
                  | Sexp.List ([ v1; v2 ]) ->
                      let v1 = Conv.string_of_sexp v1
                      and v2 = v_of_sexp v2
                      in (v1, v2)
                  | sexp -> Conv_error.tuple_of_size_n_expected _loc 2 sexp)
                 v1
             in VDict v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VSum" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = Conv.string_of_sexp v1
             and v2 = Conv.list_of_sexp v_of_sexp v2
             in VSum ((v1, v2))
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VVar" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               (match v1 with
                | Sexp.List ([ v1; v2 ]) ->
                    let v1 = Conv.string_of_sexp v1
                    and v2 = Conv.int64_of_sexp v2
                    in (v1, v2)
                | sexp -> Conv_error.tuple_of_size_n_expected _loc 2 sexp)
             in VVar v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VArrow" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in VArrow v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | Sexp.Atom "VNone" -> VNone
    | (Sexp.List (Sexp.Atom (("VSome" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = v_of_sexp v1 in VSome v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VList" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.list_of_sexp v_of_sexp v1 in VList v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom (("VTODO" as tag)) :: sexp_args) as sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = Conv.string_of_sexp v1 in VTODO v1
         | _ -> Conv_error.stag_incorrect_n_args _loc tag sexp)
    | (Sexp.List (Sexp.Atom "VUnit" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.Atom "VBool" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VFloat" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VChar" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VString" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VInt" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VTuple" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VDict" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VSum" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VVar" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VArrow" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.List (Sexp.Atom "VNone" :: _) as sexp) ->
        Conv_error.stag_no_args _loc sexp
    | (Sexp.Atom "VSome" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VList" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.Atom "VTODO" as sexp) -> Conv_error.stag_takes_args _loc sexp
    | (Sexp.List (Sexp.List _ :: _) as sexp) ->
        Conv_error.nested_list_invalid_sum _loc sexp
    | (Sexp.List [] as sexp) -> Conv_error.empty_list_invalid_sum _loc sexp
    | sexp -> Conv_error.unexpected_stag _loc sexp
and v_of_sexp sexp = v_of_sexp__ sexp


let (string_sexp_of_t: t -> string) = fun x ->
  Sexp.to_string_hum (sexp_of_t x)
let (string_sexp_of_v: v -> string) = fun x ->
  Sexp.to_string_hum (sexp_of_v x)

let (t_of_string_sexp: string -> t) = fun x ->
  t_of_sexp (Sexp.of_string x)
let (v_of_string_sexp: string -> v) = fun x ->
  v_of_sexp (Sexp.of_string x)

*)
(*****************************************************************************)
(* Json converter *)
(*****************************************************************************)

let rec json_of_v v = 
  match v with
  | VString s -> J.String s
  | VSum ((s, vs)) ->J.Array ((J.String s)::(List.map json_of_v vs ))
  | VTuple xs -> J.Array (xs +> List.map json_of_v)
  | VDict xs -> J.Object (xs +> List.map (fun (s, v) ->
      s, json_of_v v
    ))
  | VList xs -> J.Array (xs +> List.map json_of_v)
  | VNone -> J.Null
  | VSome v -> J.Array [ J.String "Some"; json_of_v v]
  | VRef v -> J.Array [ J.String "Ref"; json_of_v v]
  | VUnit -> J.Null (* ? *)
  | VBool b -> J.Bool b

  (* Note that 'Inf' can be used as a constructor but is also recognized
   * by float_of_string as a float (infinity), so when I was implementing 
   * this code by reverse engineering the generated sexp, it was important
   * to guard certain code.
   *)
  | VFloat f -> J.Float f
  | VChar c -> J.String (Common2.string_of_char c)
  | VInt i -> J.Int i
  | VTODO v1 -> J.String "VTODO"
  | VVar v1 ->
      failwith "json_of_v: VVar not handled"
  | VArrow v1 ->
      failwith "json_of_v: VArrow not handled"

(* 
 * Assumes the json was generated via 'ocamltarzan -choice json_of', which
 * have certain conventions on how to encode variants for instance.
 *)
let rec (v_of_json: Json_type.json_type -> v) = fun j ->
  match j with
  | J.String s -> VString s
  | J.Int i -> VInt i
  | J.Float f -> VFloat f
  | J.Bool b -> VBool b
  | J.Null -> raise Todo

  (* Arrays are used for represent constructors or regular list. Have to 
   * go sligtly deeper to disambiguate.
   *)
  | J.Array xs ->
      (match xs with
      (* VERY VERY UGLY. It is legitimate to have for instance tuples
       * of strings where the first element is a string that happen to
       * look like a constructor. With this ugly code we currently
       * not handle that :(
       * 
       * update: in the layer json file, one can have a filename
       * like Makefile and we don't want it to be a constructor ...
       * so for now I just generate constructors strings like
       * __Pass so we know it comes from an ocaml constructor.
       *)
       | (J.String s)::xs when s =~ "^__\\([A-Z][A-Za-z_]*\\)$" ->
           let constructor = Common.matched1 s in
           VSum (constructor, List.map v_of_json  xs)
      | ys ->
          VList (xs +> List.map v_of_json)
      )
  | J.Object flds ->
      VDict (flds +> List.map (fun (s, fld) ->
        s, v_of_json fld
      ))

let save_json file json = 
  let s = Json_out.string_of_json json in
  Common.write_file ~file s

let load_json file = 
  Json_in.load_json file

(*****************************************************************************)
(* Format pretty printers *)
(*****************************************************************************)
let add_sep xs = 
  xs +> List.map (fun x -> Right x) +> Common2.join_gen (Left ())

(* 
 * OCaml value pretty printer. A similar functionnality is provided by
 * the OCaml toplevel interpreter ('/usr/bin/ocaml') but 
 * sometimes it is useful to print values from a regular command 
 * line program. You don't always want to run the ocaml interpreter (or 
 * customized interpreter built by ocamlmktop), and type an expression
 * in to get the printed value.
 * 
 * The v_of_xxx generated code by ocamltarzan is 
 * the first part to make this possible. The function below
 * is the second part.
 * 
 * The '@[', '@,', etc are Format printf tags. See the doc of the Format
 * module in the OCaml manual to understand their meaning. Mainly, 
 * @[ and @] open and close a pretty print box, and '@ ' and '@,' 
 * are to give breaking hints to the pretty printer.
 * 
 * The output can be copy pasted in ML code directly, which can be 
 * useful when you want to pattern match over complex ocaml value.
 *)

let string_of_v v = 
  Common2.format_to_string (fun () ->
    let ppf = Format.printf in
    let rec aux v = 
      match v with
      | VUnit -> ppf "()"
      | VBool v1 ->
          if v1
          then ppf "true"
          else ppf "false"
      | VFloat v1 -> ppf "%f" v1
      | VChar v1 -> ppf "'%c'" v1
      | VString v1 -> ppf "\"%s\"" v1
      | VInt i -> ppf "%d" i
      | VTuple xs ->
          ppf "(@[";
              xs +> add_sep +> List.iter (function
              | Left _ -> ppf ",@ ";
              | Right v -> aux v
              );
          ppf "@])";
      | VDict xs ->
          ppf "{@[";
          xs +> List.iter (fun (s, v) ->
            (* less: could open a box there too? *)
            ppf "@,%s=" s;
            aux v;
            ppf ";@ ";
          );
          ppf "@]}";
          
      | VSum ((s, xs)) ->
          (match xs with
          | [] -> ppf "%s" s
          | y::ys ->
              ppf "@[<hov 2>%s(@," s;
              xs +> add_sep +> List.iter (function
              | Left _ -> ppf ",@ ";
              | Right v -> aux v
              );
              ppf "@])";
          )
          
      | VVar (s, i64) -> ppf "%s_%d" s (Int64.to_int i64)
      | VArrow v1 -> failwith "Arrow TODO"
      | VNone -> ppf "None";
      | VSome v -> ppf "Some(@["; aux v; ppf "@])";
      | VRef v -> ppf "Ref(@["; aux v; ppf "@])";
      | VList xs ->
          ppf "[@[<hov>";
          xs +> add_sep +> List.iter (function
          | Left _ -> ppf ";@ ";
          | Right v -> aux v
          );
          ppf "@]]";
      | VTODO v1 -> ppf "VTODO"
    in
    aux v
  )

(*****************************************************************************)
(* Mapper Visitor *)
(*****************************************************************************)

let map_of_unit x = ()
let map_of_bool x = x
let map_of_float x = x
let map_of_char x = x
let map_of_string (s:string) = s

let map_of_ref aref x = x (* dont go into ref *)
let map_of_option v_of_a v = 
  match v with
  | None -> None
  | Some x -> Some (v_of_a x)
let map_of_list of_a xs = 
  List.map of_a xs
let map_of_int x = x
let map_of_int64 x = x

let map_of_either _of_a _of_b =
  function
  | Left v1 -> let v1 = _of_a v1 in Left ((v1))
  | Right v1 -> let v1 = _of_b v1 in Right ((v1))

let map_of_either3 _of_a _of_b _of_c =
  function
  | Left3 v1 -> let v1 = _of_a v1 in Left3 ((v1))
  | Middle3 v1 -> let v1 = _of_b v1 in Middle3 ((v1))
  | Right3 v1 -> let v1 = _of_c v1 in Right3 ((v1))


(* this is subtle ... *)
let rec (map_v: f:( k:(v -> v) -> v -> v) -> v -> v) =
  fun ~f x ->


 let rec map_v v = 

  (* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_map.cmo  pr_o.cmo /tmp/xxx.ml  *)
  let rec k x = 
    match x with
    | VUnit -> VUnit
    | VBool v1 -> let v1 = map_of_bool v1 in VBool ((v1))
    | VFloat v1 -> let v1 = map_of_float v1 in VFloat ((v1))
    | VChar v1 -> let v1 = map_of_char v1 in VChar ((v1))
    | VString v1 -> let v1 = map_of_string v1 in VString ((v1))
    | VInt v1 -> let v1 = map_of_int v1 in VInt ((v1))
    | VTuple v1 -> let v1 = map_of_list map_v v1 in VTuple ((v1))
    | VDict v1 ->
        let v1 =
          map_of_list
            (fun (v1, v2) ->
              let v1 = map_of_string v1 and v2 = map_v v2 in (v1, v2))
            v1
        in VDict ((v1))
    | VSum ((v1, v2)) ->
        let v1 = map_of_string v1
        and v2 = map_of_list map_v v2
        in VSum ((v1, v2))
    | VVar v1 ->
        let v1 =
          (match v1 with
          | (v1, v2) ->
              let v1 = map_of_string v1 and v2 = map_of_int64 v2 in (v1, v2))
        in VVar ((v1))
    | VArrow v1 -> let v1 = map_of_string v1 in VArrow ((v1))
    | VNone -> VNone
    | VSome v1 -> let v1 = map_v v1 in VSome ((v1))
    | VRef v1 -> let v1 = map_v v1 in VRef ((v1))
    | VList v1 -> let v1 = map_of_list map_v v1 in VList ((v1))
    | VTODO v1 -> let v1 = map_of_string v1 in VTODO ((v1))
  in
  f ~k v
 in
 map_v x


(*****************************************************************************)
(* Iterator Visitor *)
(*****************************************************************************)

let v_unit x = ()
let v_bool x = ()
let v_int x = ()
let v_string (s:string) = ()
let v_ref aref x = () (* dont go into ref *)
let v_option v_of_a v = 
  match v with
  | None -> ()
  | Some x -> v_of_a x
let v_list of_a xs = 
  List.iter of_a xs

let v_either of_a of_b x = 
  match x with
  | Left a -> of_a a
  | Right b -> of_b b

let v_either3 of_a of_b of_c x = 
  match x with
  | Left3 a -> of_a a
  | Middle3 b -> of_b b
  | Right3 c -> of_c c

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)


(*****************************************************************************)
(* Templates *)
(*****************************************************************************)

(*
  match v with
  | Ocaml.VString v1 ->
      raise Todo

  | Ocaml.VSum ((s, v2)) ->
      raise Todo

  | Ocaml.VTuple xs ->
      raise Todo

  | Ocaml.VDict xs ->
      raise Todo
  | Ocaml.VList v1 ->
      raise Todo

  | Ocaml.VNone -> 
      raise Todo
  | Ocaml.VSome v1 -> 
      raise Todo

  (* *)
  | Ocaml.VUnit -> 
      raise Todo
  | Ocaml.VBool v1 ->
      raise Todo
  | Ocaml.VFloat v1 ->
      raise Todo
  | Ocaml.VChar v1 ->
      raise Todo
  | Ocaml.VInt v1 ->
      raise Todo

  | Ocaml.VVar v1 ->
      raise Todo
  | Ocaml.VArrow v1 ->
      raise Todo

  | Ocaml.VTODO v1 ->
      raise Todo

*)
