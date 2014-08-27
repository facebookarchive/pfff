(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See also prolog_code.ml!
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* for locals, but also right now for fields, globals, constants, enum, ... *)
type var = string
type func = string
type fld = string

(* _cst_xxx, _str_line_xxx, _malloc_in_xxx_line, ... *)
type heap = string
(* _in_xxx_line_xxx_col_xxx *)
type callsite = string

(* mimics datalog_code.dl top comment *)
type fact =
  | PointTo of var * heap
  | ArrayPointTo of var * heap

  | Assign of var * var
  | AssignContent of var * var
  | AssignAddress of var * var

  | AssignDeref of var * var

  | AssignLoadField of var * var * fld
  | AssignStoreField of var * fld * var
  | AssignFieldAddress of var * var * fld

  | AssignArrayElt of var * var
  | AssignArrayDeref of var * var
  | AssignArrayElementAddress of var * var

  | Parameter of func * int * var
  | Return of func * var (* ret_xxx convention *)
  | Argument of callsite * int * var
  | ReturnValue of callsite * var
  | CallDirect of callsite * func
  | CallIndirect of callsite * var

      
(*****************************************************************************)
(* Toy datalog *)
(*****************************************************************************)

let string_of_fact = function
  | PointTo (a, b) -> spf "point_to(%s, %s)" a b
  | ArrayPointTo (a, b) -> spf "array_point_to(%s, %s)" a b
  | Assign (a, b) -> spf "assign(%s, %s)" a b
  | AssignContent (a, b) -> spf "assign_content(%s, %s)" a b
  | AssignAddress (a, b) -> spf "assign_address(%s, %s)" a b
  | AssignDeref (a, b) -> spf "assign_deref(%s, %s)" a b
  | AssignLoadField (a, b, c) -> spf "assign_load_field(%s, %s, %s)" a b c
  | AssignStoreField (a, b, c) -> spf "assign_store_field(%s, %s, %s)" a b c
  | AssignFieldAddress (a, b, c) -> spf "assign_field_address(%s, %s, %s)" a b c
  | AssignArrayElt (a, b) -> spf "assign_array_elt(%s, %s)" a b
  | AssignArrayDeref (a, b) -> spf "assign_array_deref(%s, %s)" a b
  | AssignArrayElementAddress (a, b) -> spf "assign_array_element_address(%s, %s)" a b
  | Parameter (a, b, c) -> spf "parameter(%s, %d, %s)" a b c
  | Return (a, b) -> spf "return(%s, %s)" a b
  | Argument (a, b, c) -> spf "argument(%s, %d, %s)" a b c
  | ReturnValue (a, b) -> spf "call_ret(%s, %s)" a b
  | CallDirect (a, b) -> spf "call_direct(%s, %s)" a b
  | CallIndirect (a, b) -> spf "call_indirect(%s, %s)" a b


(*****************************************************************************)
(* Bddbddb *)
(*****************************************************************************)

(* see datalog_code.dl domain *)
type value = 
  | V of var
  | F of fld
  | N of func
  | I of callsite
  | Z of int

(* "V", "F", ... *)
type _domain = string

let domain_of_value = function
  | V _ -> "V"
  | F _ -> "F"
  | N _ -> "N"
  | I _ -> "I"
  | Z _ -> "Z"

type _rule = string

type _meta_fact = 
    string * value list

type _idx = (string (* metadomain*), value Common.hashset) Hashtbl.t

let meta_fact = function
  | PointTo (a, b) -> "point_to", [ V a; V b; ]
  | ArrayPointTo (a, b) -> "array_point_to", [ V a; V b; ]
  | Assign (a, b) -> "assign", [ V a; V b; ]
  | AssignContent (a, b) -> "assign_content", [ V a; V b; ]
  | AssignAddress (a, b) -> "assign_address", [ V a; V b; ]
  | AssignDeref (a, b) -> "assign_deref", [ V a; V b; ]
  | AssignLoadField (a, b, c) -> "assign_load_field", [ V a; V b; F c ]
  | AssignStoreField (a, b, c) -> "assign_store_field", [ V a; F b; V c ]
  | AssignFieldAddress (a, b, c) -> "assign_field_address", [ V a; V b; F c ]
  | AssignArrayElt (a, b) -> "assign_array_elt", [ V a; V b; ]
  | AssignArrayDeref (a, b) -> "assign_array_deref", [ V a; V b; ]
  | AssignArrayElementAddress (a, b) -> "assign_array_element_address", [ V a; V b; ]
  | Parameter (a, b, c) -> "parameter", [ N a; Z b; V c ]
  | Return (a, b) -> "return", [ N a; V b; ]
  | Argument (a, b, c) -> "argument", [ I a; Z b; V c ]
  | ReturnValue (a, b) -> "call_ret", [ I a; V b; ]
  | CallDirect (a, b) -> "call_direct", [ I a; N b; ]
  | CallIndirect (a, b) -> "call_indirect", [ I a; V b; ]


let bddbddb_of_facts facts _dir =
  let metas = facts +> List.map meta_fact in

  let hvalues = Hashtbl.create 6 in
  let hrules = Hashtbl.create 30 in

  (* build sets *)
  metas +> List.iter (fun (arule, xs) ->
    let listref =
      try Hashtbl.find hrules arule
      with Not_found -> 
        let aref = ref [] in
        Hashtbl.add hrules arule aref;
        aref
    in
    listref := xs :: !listref;

    xs +> List.iter (fun v ->
      let domain = domain_of_value v in
      let hdomain =
        try Hashtbl.find hvalues domain
        with Not_found -> Hashtbl.create 10001
      in
      Hashtbl.replace hdomain v true
    )
  );

  (* now build integer indexes *)
  raise Todo
