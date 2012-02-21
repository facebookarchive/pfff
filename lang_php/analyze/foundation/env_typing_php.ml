(* Julien Verlaguet
 *
 * Copyright (C) 2011 Facebook
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

module Int = struct type t = int let compare = (-) end
module ISet = Set.Make(Int)
module IMap = Map.Make(Int)
module SSet = Set.Make(String)
module SMap = Map.Make(String)

module Topo = Ast_php_simple_toposort
module Graph = Ast_php_simple_toposort.Graph

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  (* polymorphic type variable, 'a, 'b, ... *)
  | Tvar of int
  (* union type *)
  | Tsum of prim_ty list

  and prim_ty =
    (* A set of static strings *)
    | Tsstring of SSet.t
    (* Any abstract type, int bool string etc ... *)
    | Tabstr of string
    (* An enum of integers, class MyEnum { static const XX = 0; } *)
    | Tienum of SSet.t
    (* An enum of strings, class MyEnum { static const XX = 'foo'; } *)
    | Tsenum of SSet.t

    (* arrays are used for everything in PHP, but for the type inference
     * we actually want to differentiate those different usages, record
     * vs real array vs hash.
     *)
    | Trecord of t SMap.t
    (* An array, where we don't know what all the possible field names are. *)
    (* The SSet contains all the field names we have seen so far *)
    (* Example: $x = array('foo' => 0); $x[] = 12; will translate in
     * Tarray (SSet('foo'), int | string, int)
    *)
    | Tarray  of SSet.t * t * t

    | Tfun    of (string * t) list * t
    | Tobject of t SMap.t

    (* Same as Tobject, except that we know the set of possible classes that
       were used to instanciate the object.
     *)
    | Tclosed of SSet.t * t SMap.t

(* todo: reuse 'a cached and code database *)
type cached_ast = string

type env = {
    (* todo: use db *)
    classes: cached_ast SMap.t ref;
    funcs: cached_ast SMap.t ref;

    builtins: SSet.t ref;
    (* The graph of dependencies *)
    graph: Graph.t;

    (* The local variables environment *)
    env: t SMap.t ref;
    (* The global variable envirnoment *)
    genv: t SMap.t ref;

    (* The typing environment (pad: mapping type variables to types?) *)
    tenv: t IMap.t ref;
    (* The current substitution (for type variables) *)
    subst: int IMap.t ref;

    (* Shall we show types with the special marker? *)
    infer_types: bool;
    (* Shall we perform autocompletion? *)
    auto_complete: bool;

    (* Marker used in interactive mode *)
    marker: string;

    verbose: bool;
    (* pad: ?? *)
    depth: int;
    (* The types to show *)
    show: show ref;
    (* Are we in debug mode *)
    debug: bool;
    (* The total amount of classes/functions to type *)
    total: int ref;
    (* The total amount of classes/functions typed so far *)
    count: int ref;
    (* The internal counter for garbage collection *)
    collect_count: int ref;
    (* The cumulated garbage collection time *)
    cumul: float ref;
  }

and show =
  | Snone
  | Stype_infer of t
  | Sauto_complete of string * t
  | Sargs of t
  | Sglobal of string
  | Slocal of string * SSet.t

(*****************************************************************************)
(* Projection *)
(*****************************************************************************)

(* pad: ?? *)
let rec proj = function
  | Tabstr ("int" | "bool" | "string" | "html") -> Hashtbl.hash "string"
  | Tabstr x -> Hashtbl.hash x
  | Tsstring _ -> Hashtbl.hash "string"
  | Tienum _ -> proj (Tabstr "int")
  | Tsenum _ -> proj (Tabstr "string")
  | Trecord _ -> 2
  | Tarray  _ -> 2
  | Tfun    _ -> 3
  | Tobject _ -> 4
  | Tclosed _ -> 4

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i

let or_ l =
  let l = List.sort (fun x y -> proj x - proj y) l in
  Tsum l

let make_env () = {
  env     = ref SMap.empty;
  genv    = ref SMap.empty;

  tenv    = ref IMap.empty;
  subst   = ref IMap.empty;

  depth   = 0;
  show   = ref Snone;

  classes = ref SMap.empty;
  funcs = ref SMap.empty;

  debug = false;
  infer_types = false;
  auto_complete = false;
  verbose = true;
  marker = "JUJUMARKER";
  builtins = ref SSet.empty;

  graph = Graph.empty();
  total = ref 0;
  count = ref 0;
  collect_count = ref 0;
  cumul = ref 0.0;
}

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let pbool = Tabstr "bool"
let pint = Tabstr "int"
let pfloat = Tabstr "float"
let pstring = Tabstr "string"
let pnull = Tabstr "null"
let phtml = Tabstr "html"

let fvar() = Tvar (fresh())

let bool = Tsum [pbool]
let int = Tsum [pint]
let thtml = Tsum [phtml]
let float = Tsum [pfloat]
let string = Tsum [pstring]
let null = Tsum [pnull]

let any = Tsum []

let empty = Tsum [Trecord SMap.empty]
let array(t1, t2) = Tsum [Tarray (SSet.empty, t1, t2)]
let srecord(s, v) = Tsum [Trecord (SMap.add s v SMap.empty)]
let sobject(s, v) = Tsum [Tobject (SMap.add s v SMap.empty)]
let fun_ l b = Tsum [Tfun (List.map (fun x -> "", x) l, b)]
let afun l b = Tsum [Tfun (l, b)]
