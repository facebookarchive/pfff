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

(* This module implements a bottom-up type-inference scheme for PHP.
   Every class is first sorted in their topological order, they are then
   typed independently (hence bottom-up).
   The type representation is fairly standard (Cf Env_typing_php).
   Except for one encoding, classes.
   A class is represented as an object with a special field called __obj
   that contains the type of the instanciated object.
   Example:
   class A {
     public static function f() { }
     public function g() { }
   }
   is represented as
   Tclosed (SSet('A'), SMap(
     'g': function unit -> unit
     '__obj': SMap (
       'f' => function unit -> unit
     )
   )
*)

open Ast_php_simple
open Ast_php_simple_toposort
module A = Ast_php_simple

(*
module Int = struct type t = int let compare = (-) end
module IMap = Map.Make(Int)
module ISet = Set.Make(Int)
module SMap = Map.Make(String)
module SSet = Set.Make(String)
*)

open Env_typing_php
open Typing_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

module Builtins = struct

  let id =
    let v = Tvar (fresh()) in
    fun_ [v] v

  let array_fill =
    let v = Tvar (fresh()) in
    fun_ [int; int; v] (array (int, v))

  let array_merge =
    let v = array (Tvar (fresh()), Tvar (fresh())) in
    fun_ [v;v;v;v;v;v;v;v;v;v;v;v;v] v

  let preg_match =
    fun_ [string; string; array (int, string); int; int] int

  let strpos =
    fun_ [string; string; int] (or_ [pint; pstring])

  let array_keys =
    let v1 = Tvar (fresh()) in
    let v2 = Tvar (fresh()) in
    fun_ [array (v1, v2); v2; bool] v1

  let implode =
    fun_ [string; array (any, string)] string

  let preg_replace =
    fun_ [string; string; string; int; int] string

  let array_change_key_case =
    let v = Tvar (fresh()) in
    let ien = SSet.add "CASE_UPPER" (SSet.add "CASE_LOWER" SSet.empty) in
    let ien = Tsum [Tienum ien] in
    fun_ [array (string, v); ien] (array (string, v))

  let array_chunk =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); int; bool] (array (int, array(int, v)))

  let array_combine =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [k; v] (array (k, v))

  let array_count_values =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] (array (v, int))

  let array_fill_keys =
    let v = Tvar (fresh()) in
    let x = Tvar (fresh()) in
    fun_ [array (int, v); x] (array (v, x))

  let array_filter =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [array (k, v); any] (array (k, v))

  let array_flip =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [array (k, v)] (array (v, k))

  let array_key_exists =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [k; array (k, v)] bool

  let array_keys =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [array (k, v); v; bool] (array (int, k))

  let array_map =
    let k = Tvar (fresh()) in
    fun_ [any; array (k, any)] (array (k, any))

  let array_merge_recursive =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    let x = array (k, v) in
    fun_ [x;x;x;x;x;x;x;x;x;x;x;x] x

  let array_multisort = id

  let array_pad =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [array(k, v); int; v] (array(k, v))

  let array_pop =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] v

  let array_product =
    fun_ [array (int, int)] int

  let array_push =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] (array (int, v))

  let array_rand =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); int] v

  let array_reduce =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); any; v] v

  let array_reverse =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] (array (int, v))

  let array_search =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    fun_ [array (k, v); v; bool] k

  let array_shift =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] (array (int, v))

  let array_slice =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); int; int] (array (int, v))

  let array_splice =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); int; int; v] (array (int, v))

  let array_sum =
    let v = Tvar (fresh()) in
    fun_ [array (int, v)] v

  let array_unique =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    let x = array (k, v) in
    fun_ [x] x

  let array_unshift =
    let v = Tvar (fresh()) in
    fun_ [array (int, v);v;v;v;v;v;v;v;v;v;v;v] v

  let array_values =
    let v = Tvar (fresh()) in
    fun_ [array (any, v)] (array (int, v))

  let array_walk_recursive =
    let k = Tvar (fresh()) in
    fun_ [array (k, any); any; any] (array (k, any))

  let array_walk = array_walk_recursive

  let array_shuffle =
    let v = Tvar (fresh()) in
    let x = (array (int, v)) in
    fun_ [x] x

  let current =
    let v = Tvar (fresh()) in
    fun_ [array (any, v)] v

  let next =
    fun_ [array (any, any)] null

  let pos = current
  let prev = next
  let reset = next
  let end_ = next

  let in_array =
    let v = Tvar (fresh()) in
    fun_ [array (any, v)] bool

  let key =
    let k = Tvar (fresh()) in
    fun_ [array (k, any)] k

  let range =
    let v = Tvar (fresh()) in
    fun_ [v; v; v] (array (int, v))

  let array_diff =
    let k = Tvar (fresh()) in
    let v = Tvar (fresh()) in
    let x = array (k, v) in
    fun_ [x;x] x

  let sort =
    let v = Tvar (fresh()) in
    fun_ [array (int, v); int; bool] (array (int, v))

  let list =
    let v = Tvar (fresh()) in
    let a = array (int, v) in
    fun_ [a;a;a;a;a;a;a;a;a] any

  let super_globals =
    let h = Hashtbl.create 23 in
    let add x = Hashtbl.add h x true in
    add "$GLOBALS";
    add "$_SERVER";
    add "$_GET";
    add "$_POST";
    add "$_FILES";
    add "$_COOKIE";
    add "$_SESSION";
    add "$_REQUEST";
    add "$_ENV";
    h

  let make env =
    let add_name x = env.builtins := SSet.add ("^Fun:"^x) !(env.builtins) in
    let add x y = add_name x; GEnv.set_fun env x y in
    add "int" int;
    add "bool" bool;
    add "float" float;
    add "string" string;
    add "u" (fun_ [string] string);
    add "null" null;
    add "isset" (fun_ [any] bool);
    add "count" (fun_ [any] int);
    add "sizeof" (fun_ [any] int);
    add "id" id;
    add "array_fill" array_fill;
    add "sprintf" (fun_ [string] string);
    add "substr" (fun_ [string; int; int] string);
    add "intval" (fun_ [any] int);
    add "starts_with" (fun_ [string;string] bool);
    add "ends_with" (fun_ [string;string] bool);
    add "array_merge" array_merge;
    add "preg_match" preg_match;
    add "preg_replace" preg_replace;
    add "strpos" strpos;
    add "time" (fun_ [] int);
    add "array_keys" array_keys;
    add "implode" implode;
    add "empty" (fun_ [any] bool);
    add "unset" (fun_ [any] null);
    add "trim" (fun_ [string; string] string);
    add "get_class" (fun_ [any] string);
    add "str_replace" (fun_ [string; string; string] string);
    add "strlen" (fun_ [string] int);
    add "is_array" (fun_ [array (any, any)] bool);
    add "is_string" (fun_ [string] bool);
    add "is_bool" (fun_ [bool] bool);
    add "is_int" (fun_ [int] int);
    add "is_float" (fun_ [float] bool);
    add "is_scalar" (fun_ [or_ [pint;pfloat;pbool]] bool);
    add "is_object" (fun_ [Tsum [Tobject (SMap.empty)]] bool);
    add "is_numeric" (fun_ [or_ [pint;pfloat]] bool);
    add "array_change_key_case" array_change_key_case;
    add "array_chunk" array_chunk;
    add "array_combine" array_combine;
    add "array_count_values" array_count_values;
    add "array_fill_keys" array_fill_keys;
    add "array_filter" array_filter;
    add "array_flip" array_flip;
    add "array_key_exists" array_key_exists;
    add "array_keys" array_keys;
    add "array_map" array_map;
    add "array_merge_recursive" array_merge_recursive;
    add "array_multisort" array_multisort;
    add "array_pad" array_pad;
    add "array_pop" array_pop;
    add "array_product" array_product;
    add "array_push" array_push;
    add "array_rand" array_rand;
    add "array_reduce" array_reduce;
    add "array_reverse" array_reverse;
    add "array_search" array_search;
    add "array_shift" array_shift;
    add "array_slice" array_slice;
    add "array_splice" array_splice;
    add "array_sum" array_sum;
    add "array_unique" array_unique;
    add "array_unshift" array_unshift;
    add "array_values" array_values;
    add "array_walk_recursive" array_walk_recursive;
    add "array_walk" array_walk;
    add "array_shuffle" array_shuffle;
    add "current" current;
    add "next" next;
    add "pos" pos;
    add "prev" prev;
    add "reset" reset;
    add "end" end_;
    add "in_array" in_array;
    add "key" key;
    add "range" range;
    add "array_diff" array_diff;
    add "explode" (fun_ [string; string; int] (array (int, string)));
    add "max" (fun_ [] any);
    add "chr" (fun_ [int] string);
    add "strtoupper" (fun_ [string] string);
    add "floor" (fun_ [float] int);
    add "strtotime" (fun_ [string; int] int);
    add "microtime" (fun_ [bool] (or_ [pint; pfloat]));
    add "echo" (fun_ [string] null);
    add "exit" (fun_ [int] null);
    add "print" (fun_ [string] null);
    add "json_encode" (fun_ [string] string);
    add "date" (fun_ [string; int] string);
    add "strftime" (fun_ [string; int] string);
    add "sort" sort;
    add "round" (fun_ [float] int);
    add "join" implode;
    add "htmlize" (fun_ [thtml] string);
    add "txt2html" (fun_ [thtml; bool] string);
    add "list" list;

end

module FindCommonAncestor = struct

  exception Found of string

  let rec class_match env cand acc id =
    Classes.mem env id &&
    let c = Classes.get env id in
    match c.c_extends with
    | [] -> false
    | l when List.mem cand l -> true
    | l -> List.fold_left (class_match env cand) acc l

  let rec get_candidates env acc id =
    let acc = SSet.add id acc in
    if not (Classes.mem env id) then acc else
    let c = Classes.get env id in
    List.fold_left (get_candidates env) acc c.c_extends

  let go env ss =
    let l = SSet.fold (fun x y -> x :: y) ss [] in
    let cands = List.fold_left (get_candidates env) SSet.empty l in
    try SSet.iter (
    fun cand ->
      let all_match = List.fold_left (class_match env cand) false l in
      if all_match then raise (Found cand)
   ) cands;
    None
    with Found c -> Some c

end

module Print2 = struct

  let rec ty env penv stack depth x =
    match x with
    | Tvar n ->
        let n = Subst.get env n in
        let t = TEnv.get env n in
        if ISet.mem n stack then begin
          Pp.print penv (string_of_int n);
          Pp.print penv "&";
        end
        else begin
          let stack = ISet.add n stack in
          ty env penv stack depth t
        end
    | Tsum [] -> Pp.print penv "_"
    | Tsum [x] -> prim_ty env penv stack depth x
    | Tsum l ->
        Pp.list penv (fun penv -> prim_ty env penv stack depth) "(" l " |" ")"

  and prim_ty env penv stack depth = function
    | Tabstr s -> Pp.print penv s
    | Tsstring s -> Pp.print penv "string"
    | Tienum _
    | Tsenum _ ->
(*        let l = SSet.fold (fun x acc -> Tabstr x :: acc) s [] in *)
        Pp.print penv "enum"
    | Trecord m ->
        let depth = depth + 1 in
        Pp.print penv "array";
        if depth >= 2
        then Pp.print penv "(...)"
        else
          let l = SMap.fold (fun x y l -> (x, y) :: l) m [] in
          Pp.list penv (fun penv -> print_field env " => " penv stack depth) "(" l ";" ")";
    | Tarray (_, t1, t2) ->
        Pp.print penv "array(";
        Pp.nest penv (fun penv ->
          ty env penv stack depth t1;
          Pp.print penv " => ";
          Pp.nest penv (fun penv ->
            ty env penv stack depth t2));
        Pp.print penv ")";
    | Tfun (tl, t) ->
        Pp.print penv "fun ";
        Pp.list penv (
        fun penv (s, x) ->
          ty env penv stack depth x;
          if s = "" then () else
          (Pp.print penv " ";
           Pp.print penv s)
       ) "(" tl "," ")";
        Pp.print penv " -> ";
        Pp.nest penv (fun penv ->
          ty env penv stack depth t)
    | Tobject m ->
        let depth = depth + 1 in
        Pp.print penv "object";
        if depth >= 3
        then Pp.print penv "(...)"
        else
          let l = SMap.fold (fun x y l -> (x, y) :: l) m [] in
          Pp.list penv (fun penv -> print_field env ": " penv stack depth) "(" l ";" ")";
    | Tclosed (s, _) ->
        if SSet.cardinal s = 1 then Pp.print penv (SSet.choose s) else
        (match FindCommonAncestor.go env s with
        | None ->
            let l = SSet.fold (fun x acc -> x :: acc) s [] in
            Pp.list penv (Pp.print) "(" l "|" ")";
        | Some s -> Pp.print penv s)

  and print_field env sep penv stack depth (s, t) =
    Pp.print penv s;
    Pp.print penv sep;
    Pp.nest penv (fun penv ->
      ty env penv stack depth t)

  let genv env =
    let penv = Pp.empty print_string in
    GEnv.iter env (
    fun x t ->
      if not (SSet.mem x !(env.builtins)) then begin
        Pp.print penv x; Pp.print penv " = ";
        ty env penv ISet.empty 0 t;
        Pp.newline penv;
      end
       )

  let penv env =
    genv env

  let args o env t =
    match Fun.get_args env ISet.empty t with
    | [] -> ()
    | tl ->
        let penv = Pp.empty o in
        let stack = ISet.empty in
        let depth = 1000 in
        Pp.list penv (
        fun penv (s, x) ->
          if s = "" then
            ty env penv stack depth x
          else begin
            if x = Tsum [] then () else ty env penv stack depth x;
            (Pp.print penv " ";
             Pp.print penv s)
          end
       ) "(" tl "," ")"

  let rec get_fields vim_mode env stack acc = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack then SSet.empty else
        let stack = ISet.add n stack in
        let t = TEnv.get env n in
        get_fields vim_mode env stack acc t
    | Tsum l -> List.fold_left (get_prim_fields vim_mode env stack) acc l

  and get_prim_fields vim_mode env stack acc = function
    | Tabstr _ -> acc
    | Tsstring s -> SSet.union s acc
    | Tienum s
    | Tsenum s -> SSet.union s acc
    | Tobject m ->
        SMap.fold (
        fun x t acc ->
          if x = "__obj" then acc else
          let x =
            if not vim_mode && Fun.is_fun env ISet.empty t
            then
              (match Fun.get_args env ISet.empty t with
              | [] -> x^"()"
              | _ ->
                  x^"("
              )
            else x
          in
          SSet.add x acc
       ) m acc
    | Tclosed (s, m) ->
        let acc =
          try
            if SSet.cardinal s = 1
            then (match GEnv.get_class env (SSet.choose s) with
            | Tsum [Tobject m] ->
                get_fields vim_mode env stack acc (SMap.find "__obj" m)
            | _ -> acc)
            else acc
          with _ -> acc
        in
        get_prim_fields vim_mode env stack acc (Tobject m)
    | Trecord m ->
        SMap.fold (fun x _ acc -> SSet.add x acc) m acc
    | Tarray (s, t, _) ->
        let acc = SSet.union s acc in
        let acc = get_fields vim_mode env stack acc t in
        acc
    | Tfun _ -> acc


  let get_fields vim_mode env t =
    let acc = get_fields vim_mode env ISet.empty SSet.empty t in
    acc

end

module Print = struct

  let rec print o env stack = function
    | Tvar n ->
        let n = Subst.get env n in
        if IMap.mem n stack
        then if env.debug then (o "rec["; o (string_of_int n); o "]") else o "rec"
        else if TEnv.mem env n
        then begin
          if env.debug then (o "["; o (string_of_int n); o "]");
          let stack = IMap.add n true stack in
          print o env stack (TEnv.get env n)
        end
        else
            (o "`"; o (string_of_int n))
    | Tsum [] -> o "*"
    | Tsum l ->
        sum o env stack l

  and print_prim o env stack = function
    | Tabstr x -> o x
    | Tienum s -> o "ienum{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Tsstring s -> o "cstring{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Tsenum s -> o "senum{"; SSet.iter (fun x -> o " | "; o x) s; o " | }"
    | Trecord m ->
        o "r{";
        SMap.iter (
        fun x t ->
          o x;
          if env.debug then
            (o ":"; print o env stack t);
          o ","
       ) m;
        o "}";
    | Tarray (_, t1, t2) ->
        o "array(";
        print o env stack t1;
        o " => ";
        print o env stack t2;
        o ")"
    | Tfun (l, t) ->
        o "(";
        list o env stack l;
        o " -> ";
        print o env stack t;
        o ")"
    | Tobject m ->
        o "obj"; print_prim o env stack (Trecord m)
    | Tclosed (_, m) -> print_prim o env stack (Tobject m)

  and list o env stack l =
    match l with
    | [] -> o "()"
    | [_, x] -> print o env stack x
    | (_, x) :: rl -> print o env stack x; o ", "; list o env stack rl

  and sum o env stack l =
    match l with
    | [] -> ()
    | [x] -> print_prim o env stack x
    | x :: rl -> print_prim o env stack x; o " | "; sum o env stack rl

  let dd env x =
    print print_string env IMap.empty x;
    print_string "\n"

  let genv env =
    GEnv.iter env (
    fun x t ->
      if not (SSet.mem x !(env.builtins)) then begin
        print_string x; print_string " = ";
        print print_string env IMap.empty t;
        print_string "\n";
      end
       ) ; flush stdout

  let penv env =
    Printf.printf "*******************************\n";
    genv env;
    if env.debug then
      SMap.iter (
      fun x t ->
        if not (SSet.mem x !(env.builtins)) then begin
          print_string x; print_string " = ";
          print print_string env IMap.empty t;
          print_string "\n";
        end
     ) !(env.env);
    flush stdout

  let show_type env o t =
    Print2.ty env (Pp.empty o) ISet.empty 0 t;
    o "\n"
end

module Instantiate = struct

  let rec get_vars env stack subst = function
    | Tvar n ->
        let n = Subst.get env n in
        (match TEnv.get env n with
        | _ when ISet.mem n stack ->
            ISet.add n subst
        | Tsum [] -> ISet.add n subst
        | t -> get_vars env (ISet.add n stack) subst t
        )
    | Tsum l -> List.fold_left (get_prim_vars env stack) subst l

  and get_prim_vars env stack subst = function
    | Trecord m ->
        SMap.fold (
        fun _ t subst ->
          get_vars env stack subst t
       ) m subst
    | Tarray (_, t1, t2) ->
        let subst = get_vars env stack subst t1 in
        let subst = get_vars env stack subst t2 in
        subst
    | _ -> subst

  let rec replace_vars env stack subst is_left = function
    | Tvar n ->
        let n = Subst.get env n in
        if IMap.mem n subst then Tvar (IMap.find n subst) else
        (match TEnv.get env n with
        | _ when ISet.mem n stack -> Tsum []
        | t -> replace_vars env (ISet.add n stack) subst is_left t
        )
    | Tsum l when List.length l > 1 -> Tsum []
    | Tsum l -> Tsum (List.map (replace_prim_vars env stack subst is_left) l)

  and replace_prim_vars env stack subst is_left = function
    | Trecord m -> Trecord (SMap.map (replace_vars env stack subst is_left) m)
    | Tarray (s, t1, t2) ->
        let t1 = replace_vars env stack subst is_left t1 in
        let t2 = replace_vars env stack subst is_left t2 in
        Tarray (s, t1, t2)
    | x -> x

  let rec ty env stack t =
    match t with
    | Tvar x ->
        let x = Subst.get env x in
        let t = TEnv.get env x in
        if ISet.mem x stack then Tvar x else
        let stack = ISet.add x stack in
        TEnv.set env x (ty env stack t);
        Tvar x
    | Tsum tyl -> Tsum (List.map (prim_ty env stack) tyl)

  and prim_ty env stack = function
    | Tfun (tl, t) ->
        let argl = List.map snd tl in
        let vars = List.fold_left (get_vars env ISet.empty) ISet.empty argl in
        let vars = ISet.fold (fun x acc -> IMap.add x (fresh()) acc) vars IMap.empty in
        Tfun (List.map (fun (s, x) -> s, replace_vars env ISet.empty vars true x) tl,
              replace_vars env ISet.empty vars false t)
    | x -> x

  let rec approx env stack t =
    match t with
    | Tvar x ->
        let x = Subst.get env x in
        let t = TEnv.get env x in
        if ISet.mem x stack then Tvar x else
        let stack = ISet.add x stack in
        approx env stack t
    | Tsum [x] -> Tsum (approx_prim_ty env stack x)
    | _ -> Tsum []

  and approx_prim_ty env stack = function
    | Tarray (s, t1, t2) -> [Tarray (s, approx env stack t1, approx env stack t2)]
    | Tobject _
    | Tfun _ -> []
    | x -> [x]

end

module Generalize = struct

  let rec ty env stack = function
    | Tvar n ->
        let n = Subst.get env n in
        if ISet.mem n stack then Tsum [] else
        (match TEnv.get env n with
        | Tsum [Tabstr "null"]
        | Tsum [] -> Tvar n
        | t ->
            ty env (ISet.add n stack) t
        )
    | Tsum l -> Tsum (List.map (prim_ty env stack) l)

  and prim_ty env stack = function
    | Tarray (s, t1, t2) -> Tarray (s, ty env stack t1, ty env stack t2)
    | Tfun (tl, t) -> Tfun (List.map (fun (s, x) -> s, ty env stack x) tl, ty env stack t)
    | x -> x

end
(*****************************************************************************)
(* Unification *)
(*****************************************************************************)

module Type = struct

  let rec mixed has_array has_object l =
    match l with
    | [] -> has_array && has_object
    | (Tobject _ | Tclosed _) :: rl -> mixed has_array true rl
    | (Trecord _ | Tarray _) :: rl -> mixed true has_object rl
    | _ :: rl -> mixed has_array has_object rl

  let mixed l = mixed false false l

  let rec unify env t1 t2 =
    if t1 == t2 then t1 else
    match t1, t2 with
    | Tsum ([] | [Tabstr "null"]), x
    | x, Tsum ([] | [Tabstr "null"]) -> x
    | x, y when x == y -> x
    | Tvar x, Tvar y when x = y -> t1
    | Tvar n1, Tvar n2 -> unify_vars env n1 n2
    | (Tvar n as x), y | y, (Tvar n as x) ->
        let n' = fresh() in
        TEnv.set env n' y;
        unify env x (Tvar n')
    | Tsum x, Tsum y ->
        let l = unify_sum env x y in
        if mixed l
        then Tsum []
        else if List.length l > 3
        then Tsum []
        else Tsum l
(*        (match l with
        | []
        | [_] -> Tsum l
        | _ -> Tpoly
        ) *)

  and unify_vars env n1 n2 =
(*    let n1' = Subst.get env n1 in
    let n2' = Subst.get env n2 in
    if n1' = n2' then Tvar n1' else
    let t1 = TEnv.get env n1' in
    let t2 = TEnv.get env n2' in
    let n = fresh() in
    Subst.replace env n1 n;
    Subst.replace env n2 n;
    let t = unify env t1 t2 in
    let n = Subst.get env n in
    TEnv.set env n t;
    (Tvar n) *)
    let n1' = Subst.get env n1 in
    let n2' = Subst.get env n2 in
    if n1' = n2' then Tvar n1' else
    let t1 = TEnv.get env n1' in
    let t2 = TEnv.get env n2' in
    let n = fresh() in
    Subst.replace env n1 n;
    Subst.replace env n2 n;
    let t = unify env t1 t2 in
    let n = Subst.get env n in
    TEnv.set env n t;
    (Tvar n)

  and unify_ env t1 t2 =
    match t1, t2 with
    | Tsstring s1, Tsstring s2 when s1 == s2 -> t1
    | Tsstring s1, Tsstring s2 ->
        let s = SSet.union s1 s2 in
        if SSet.cardinal s > 200
        then Tabstr "string"
        else Tsstring s1
    | (Tsstring _ as t), _ | _, (Tsstring _ as t) -> t
    | Tienum s1, Tienum s2 when s1 == s2 -> t1
    | Tienum s1, Tienum s2 -> Tienum (SSet.union s1 s2)
    | Tienum _, t
    | t, Tienum _ -> unify_ env t (Tabstr "int")
    | Tsenum s1, Tsenum s2 when s1 == s2 -> t1
    | Tsenum s1, Tsenum s2 -> Tsenum (SSet.union s1 s2)
    | Tsenum _, t
    | t, Tsenum _ -> unify_ env (Tabstr "string") t
    | Tabstr "bool", t (* We want them in this order bool < int < string < html *)
    | t, Tabstr "bool" -> t
    | Tabstr "int", t
    | t, Tabstr "int" -> t
    | Tabstr "string", t
    | t, Tabstr "string" -> t
    | Tabstr _, _ -> t1
    | Trecord s1, Trecord s2 ->
        if s1 == s2 then t1 else
        Trecord (unify_map env s1 s2)
    | Trecord x, t | t, Trecord x ->
        if SMap.is_empty x
        then t
        else
          let v = SMap.fold (fun _ t acc -> unify env t acc) x any in
          let s = SMap.fold (fun x _ acc -> SSet.add x acc) x SSet.empty in
          unify_ env (Tarray (s, string, v)) t
    | Tarray (s1, t1, t2), Tarray (s2, t3, t4) ->
        let s = SSet.union s1 s2 in
        Tarray (s, unify env t1 t3, unify env t2 t4)
    | Tfun (l1, t1), Tfun (l2, t2) ->
        let l = unifyl env l1 l2 in
        let t = unify env t1 t2 in
        Tfun (l, t)
    | Tobject o, Tclosed (s, c)
    | Tclosed (s, c), Tobject o ->
        let o = unify_map env o c in
        Tclosed (s, o)
    | Tobject o1, Tobject o2 ->
        if o1 == o2 then t1 else
        Tobject (unify_map env o1 o2)
    | Tclosed (s1, c1), Tclosed (s2, c2) ->
        let c1 = SMap.fold (
          fun x t acc ->
            if SMap.mem x c2
            then SMap.add x t acc
            else acc
         ) c1 SMap.empty in
        let c2 = SMap.fold (
          fun x t acc ->
            if SMap.mem x c1
            then SMap.add x t acc
            else acc
         ) c2 SMap.empty in
        Tclosed (SSet.union s1 s2, unify_map env c1 c2)
    | _ -> assert false

  and unifyl env l1 l2 =
    match l1, l2 with
    | [], l | l, [] -> l
    | (s1, x1) :: rl1, (s2, x2) :: rl2 ->
        let s = if s1 = s2 then s1 else "" in
        (s, unify env x1 x2) :: unifyl env rl1 rl2

  and unify_map env s1 s2 =
    SMap.fold (
    fun x t acc ->
      if SMap.mem x s2
      then
        let t = unify env t (SMap.find x s2) in
        SMap.add x t acc
      else SMap.add x t acc
   ) s1 s2

  and unify_sum env l1 l2 =
    match l1, l2 with
    | [], l
    | l, [] -> l
    | x1 :: rl1, x2 :: rl2 ->
        let c1 = proj x1 in
        let c2 = proj x2 in
        let c = c1 - c2 in
        if c < 0
        then x1 :: unify_sum env rl1 l2
        else if c > 0
        then x2 :: unify_sum env l1 rl2
        else unify_ env x1 x2 :: unify_sum env rl1 rl2


end

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

module Collect = struct

  type mem = {
      tys: (t, t) Hashtbl.t;
      prims: (prim_ty, prim_ty) Hashtbl.t;
    }

  let rec ty env subst tenv mem depth t =
(*    try Hashtbl.find mem.tys t
    with Not_found ->
      let t' = *)
        match t with
        | Tvar n ->
            let n = Subst.get env n in
            if IMap.mem n !subst then Tvar (IMap.find n !subst) else
            let v = fresh() in
            subst := IMap.add n v !subst;
            let t = ty env subst tenv mem depth (TEnv.get env n) in
            tenv := IMap.add v t !tenv;
            Tvar v
        | Tsum l -> Tsum (List.map (prim_ty env subst tenv mem depth) l)
(*      in
      Hashtbl.add mem.tys t t';
      t' *)

  and prim_ty env subst tenv mem depth t =
    try Hashtbl.find mem.prims t
    with Not_found ->
      if depth > 3 then t else
      let t' =
        match t with
        | Tabstr _
        | Tienum _
        | Tsstring _
        | Tsenum _ as x -> x
        | Trecord m -> Trecord (SMap.map (ty env subst tenv mem depth) m)
        | Tarray (s, t1, t2) -> Tarray (s, ty env subst tenv mem depth t1, ty env subst tenv mem depth t2)
        | Tfun (tl, t) -> Tfun (List.map (fun (s, x) -> s, ty env subst tenv mem depth x) tl, ty env subst tenv mem depth t)
        | Tobject m ->
            let m = SMap.map (ty env subst tenv mem depth) m in
            Tobject m
        | Tclosed (s, m) -> Tclosed (s, SMap.map (ty env subst tenv mem depth) m)
      in
      Hashtbl.add mem.prims t t';
      t'

  let ty count env subst tenv mem t =
    incr count;
    if !count mod 100 = 0
    then (Printf.printf "Collected [%d/%d]\n" !count !(env.count); flush stdout)
    else ();
    ty env subst tenv mem 0 t

  let run env =
    Printf.printf "Collecting [Cumulated: %f]: " !(env.cumul); flush stdout;
    let t = Sys.time() in
    let subst = ref IMap.empty in
    let tenv = ref IMap.empty in
    let ty = ty (ref 0) in
    let mem = { tys = Hashtbl.create 1024; prims = Hashtbl.create 1024 } in
    let lenv = SMap.map (ty env subst tenv mem) !(env.env) in
    let genv = SMap.map (ty env subst tenv mem) !(env.genv) in
    env.tenv := !tenv;
    env.subst := !subst;
    env.genv := genv;
    env.env := lenv;
    Printf.printf "Compacting: "; flush stdout;
    Gc.compact();
    Printf.printf "DONE\n"; flush stdout;
    let t = Sys.time() -. t in
    env.cumul := t +. !(env.cumul);
    env.collect_count := 0;
    Printf.printf "%f\n" t; flush stdout

  let collect env =
    incr env.collect_count;
    if !(env.collect_count) >= 100000
    then run env
    else ()
end

let rec program env =
  Printf.printf "Topological sort:  "; flush stdout;
  let l = TopoSort.sort env.graph in
  Printf.printf "DONE\n"; flush stdout;
  env.total := List.length l;
  List.iter (type_def env) l;
(*  Classes.iter (class_def env);
  Functions.iter (func_def env); *)
  if env.debug then Print2.penv env;
  let oc = open_out "typing_env.bin" in
  Printf.printf "Saving environment (typing_env.bin): "; flush stdout;
  Collect.run env;
  GEnv.save env oc;
  close_out oc;
  Printf.printf "DONE\n"

and type_def env x =
  if Classes.mem env x && not (GEnv.mem_class env x)
  then (class_id env x);
  if Functions.mem env x && not (GEnv.mem_fun env x)
  then (func_id env x)
  else ()

and decls env stl =
  List.iter (
  function
    | ClassDef cd ->
        Graph.class_def env.graph cd;
        Classes.add env (A.unwrap cd.c_name) cd
    | FuncDef fd ->
        Graph.func_def env.graph fd;
        Functions.add env (A.unwrap fd.f_name) fd
    | ConstantDef _ ->
        raise Common.Todo
    | _ -> ()
 ) stl

and stmtl env l =
  List.iter (stmt env) l

and stmt env = function
  | Expr e -> iexpr env e
  | Block stl -> stmtl env stl
  | If (e, st1, st2) ->
      iexpr env e;
      stmt env st1;
      stmt env st2
  | While (e, stl) ->
      iexpr env e;
      stmtl env stl
  | Do (stl, e) ->
      stmtl env stl;
      iexpr env e
  | For (el1, el2, el3, stl) ->
      iexprl env el1;
      iexprl env el2;
      iexprl env el3;
      stmtl env stl
  | Switch (e, cl) ->
      let t = expr env e in
      casel env t cl
  | Foreach (e1, e2, eopt, stl) ->
      let a = expr env e1 in
      let a' =
        match eopt with
        | None -> array (Tvar (fresh()), expr env e2)
        | Some v -> array (expr env e2, expr env v)
      in
      let _ = Type.unify env a a' in
      stmtl env stl
  | Return None -> ()
  | Return (Some e) ->
      iexpr env (Assign (None, Id (wrap "$;return"), e))
  | Break eopt -> expr_opt env eopt
  | Continue eopt -> expr_opt env eopt
  | Throw e -> iexpr env e
  | Try (stl, c, cl) ->
      stmtl env stl;
      catch env c;
      catchl env cl
  | StaticVars svarl ->
      List.iter (
      fun (s, e) ->
        match e with
        | None -> ()
        | Some e ->
            iexpr env (Assign (None, Id s, e))
     ) svarl
  | Global el ->
      List.iter (
      function
        | Id (x, tok) ->
            let gid = String.sub x 1 (String.length x -1) in
            let gl = Array_get (Id (wrap "$GLOBALS"), Some (String gid)) in
            let assign = Assign (None, Id (x, tok), gl) in
            iexpr env assign
        | e -> iexpr env e
     ) el
  | ClassDef cd ->
      class_def env cd
  | FuncDef fd ->
      func_def env fd
  | ConstantDef cd ->
      raise Common.Todo

and expr_opt env = function
  | None -> ()
  | Some x -> iexpr env x

and casel env t l = List.iter (case env t) l

and case env t = function
  | Case (e, stl) ->
      let t' = expr env e in
      let _ = Type.unify env t t' in
      stmtl env stl
  | Default stl -> stmtl env stl

and catchl env l = List.iter (catch env) l
and catch env (_, _, stl) = stmtl env stl

and exprl env l = List.map (expr env) l
and iexprl env l = ignore (exprl env l)

and iexpr env e = ignore (expr env e)

and expr env e =
  expr_ env false e

and expr_ env lv = function
  | Int _ -> int
  | Double _ -> float
  | String s when env.auto_complete && has_marker env s ->
      let t = Tvar (fresh()) in
      env.show := Sauto_complete (s, t);
      t
  | String s when String.contains s '<' -> thtml
  | String s -> Tsum [Tsstring (SSet.singleton s)]
  | Guil el ->
      List.iter (encaps env) el;
      string
  | Id (("true" | "false"),_) -> bool
  | Id (s, tok) ->
      let is_marked = has_marker env s in
      if env.infer_types && is_marked
      then begin
        let s = get_marked_id env s in
        let t = expr env (Id (s, tok)) in
        env.show := Stype_infer t;
        t
      end
      else if env.auto_complete && is_marked
      then begin
        if s.[0] = '$'
        then
          let locals = SMap.fold (fun x _ acc -> SSet.add x acc) !(env.env) SSet.empty in
          env.show := Slocal (get_marked_id env s, locals)
        else env.show := Sglobal (get_marked_id env s);
        any
      end
      else if s.[0] = '$' || Env.mem env s
      then Env.get env s
      else if GEnv.mem_fun env s
      then GEnv.get_fun env s
      else if GEnv.mem_class env s
      then GEnv.get_class env s
      else if Classes.mem env s || Functions.mem env s
      then (type_def env s; expr env (Id (s, tok)))
      else begin
        if env.debug then
          (Printf.printf "Unknown identifier: %s\n" s; flush stdout);
        any
      end
  | This -> expr env (Id (wrap "$this"))
  | Array_get (e, None) ->
      let t1 = expr env e in
      let v = Tvar (fresh()) in
      let t2 = array (int, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Array_get (e, Some (Id (s,_))) when s.[0] <> '$' ->
      expr env (Array_get (e, Some (String s)))
  | Array_get (Id (s,_), Some (String x))
      when Hashtbl.mem Builtins.super_globals s ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_global env s in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (x, v) in
      let _ = Type.unify env t1 t2 in
      Instantiate.approx env ISet.empty v
  | Array_get (e, Some (String s))->
      let marked = env.auto_complete && has_marker env s in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (s, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = srecord (s, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Call (Id (("idx" | "edx" | "adx" | "sdx"),_), (e :: k :: r)) ->
      let e = expr env (Array_get (e, Some k)) in
      (match r with
      | [] -> e
      | x :: _ -> Type.unify env (expr env x) e
      )
  | Array_get (e, Some k) ->
      let t1 = expr env e in
      let k = expr env k in
      let v = Tvar (fresh()) in
      let t2 = array (k, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get (Id (c,_), Id (x,_)) when c <> special "self" && c <> special "parent" ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = GEnv.get_class env c in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get (e, Id (x,_))
  | Obj_get (e, Id (x,_)) ->
      let marked = env.auto_complete && has_marker env x in
      let t1 = expr env e in
      if marked then (env.show := Sauto_complete (x, t1); any) else
      let v = Tvar (fresh()) in
      let t2 = sobject (x, v) in
      let _ = Type.unify env t1 t2 in
      v
  | Class_get _
  | Obj_get _ -> any
  | Assign (None, e1, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      let t = Type.unify env t1 t2 in
      t
  | Assign (Some bop, e1, e2) ->
      expr env (Assign (None, e1, Binop (bop, e1, e2)))
  | ConsArray avl ->
      let t = Tvar (fresh()) in
      let t = List.fold_left (array_value env) t avl in
      t
  | Infix (_, e) -> expr env e
  | Postfix (_, e) -> expr env e
  | Binop (bop, e1, e2) ->
      let t1 = expr env e1 in
      let t2 = expr env e2 in
      binaryOp env t1 t2 bop
  | Unop (uop, e) ->
      let _ = expr env e in
      unaryOp uop
  | Call (e, [Id ("JUJUMARKER",_)]) ->
      env.show := Sargs (expr env e);
      any
  | Call (e, el) ->
      let f = expr env e in
      let f = Instantiate.ty env ISet.empty f in
      let v = Tvar (fresh()) in
      let f' = fun_ (exprl env el) v in
      let _ = Type.unify env f f' in
      v
  | Ref e -> expr env e
  | Xhp x ->
      xml env x;
      let name = List.fold_right (fun x acc -> x^":"^acc) x.xml_tag "" in
      let t = expr env (New (Id (wrap name), [])) in
      t
  | List el ->
      let t = Tvar (fresh()) in
      let el = List.map (expr env) el in
      let t = List.fold_left (Type.unify env) t el in
      array (int, t)
  | New (Id (x,_), _) when env.auto_complete && has_marker env x ->
      env.show := Sglobal (get_marked_id env x);
      any
  | New (x, el) ->
      let v = "$;tmp"^(string_of_int (fresh())) in
      let obj = Class_get (x, Id (wrap "__obj")) in
      iexpr env (Assign (None, Id (wrap v), obj));
      iexpr env (Call (Obj_get (obj, Id (wrap "__construct")), el));
      let t = expr env (Id (wrap v)) in
      let set = match x with
        | Id (c,_) when c.[0] <> '$' -> SSet.singleton c
        | _ -> SSet.empty
      in
      Type.unify env t (Tsum [Tclosed (set, SMap.empty)])
  | InstanceOf (e1, e2) ->
      iexpr env e1;
      iexpr env e2;
      bool
  | CondExpr (e1, e2, e3) ->
      iexpr env e1;
      let e2 = expr env e2 in
      let e3 = expr env e3 in
      Type.unify env e2 e3
  | Cast (pty, e) ->
      iexpr env e;
      ptype env pty
  | Lambda _ -> (* TODO *)
      any

and encaps env e =
  let t = expr env e in
  ignore (Type.unify env t string)

and ptype env = function
  | Ast_php.BoolTy -> bool
  | Ast_php.IntTy -> int
  | Ast_php.DoubleTy -> float
  | Ast_php.StringTy -> string
  | Ast_php.ArrayTy -> Tsum [Trecord SMap.empty]
  | Ast_php.ObjectTy -> Tsum [Tobject SMap.empty]

and array_value env t = function
  | Aval e ->
      let t' = array (int, expr env e) in
      Type.unify env t t'
  | Akval (String s, e) ->
      let t' = srecord (s, (expr env e)) in
      Type.unify env t t'
  | Akval (e1, e2) ->
      let t' = array (expr env e1, expr env e2) in
      Type.unify env t t'

and binaryOp env t1 t2 = function
  | Ast_php.Arith _ ->
      let t = Type.unify env t1 t2 in
      t
  | Ast_php.Logical lop ->
      logicalOp env t1 t2 lop;
      bool
  | Ast_php.BinaryConcat ->
      Type.unify env t1 t2

and logicalOp env t1 t2 = function
  | Ast_php.Inf | Ast_php.Sup | Ast_php.InfEq | Ast_php.SupEq
  | Ast_php.Eq | Ast_php.NotEq
  | Ast_php.Identical | Ast_php.NotIdentical ->
      ignore (Type.unify env t1 t2)
  | Ast_php.AndLog | Ast_php.OrLog | Ast_php.XorLog
  | Ast_php.AndBool | Ast_php.OrBool ->
      ()

and unaryOp = function
  | Ast_php.UnPlus | Ast_php.UnMinus | Ast_php.UnTilde -> int
  | Ast_php.UnBang -> bool

and xhp env = function
  | XhpText _ -> ()
  | XhpExpr e -> ignore (expr env e)
  | XhpXml x -> xml env x

and xml env x =
  List.iter (fun (_, x) -> xhp_attr env x) x.xml_attrs;
  List.iter (xhp env) x.xml_body

and xhp_attr env = function
  | Guil el -> List.iter (encaps env) el
  | e ->
      let t = expr env e in
      ignore (Type.unify env t string)

and func_id env fname =
  if GEnv.mem_fun env fname then () else
  try func_def env (Functions.get env fname)
  with Not_found ->
    GEnv.set_fun env fname (Tvar (fresh()));
    if env.verbose then begin
    Printf.printf "Function not found: %s\n" fname; flush stdout;
    end

and func_def env fd =
  if GEnv.mem_fun env (Ast.unwrap fd.f_name) then () else
  if env.verbose then begin
    incr env.count;
  Printf.printf "Typing function(%d/%d)[%d]: %s\n" !(env.count) !(env.total) env.depth (Ast.unwrap fd.f_name); flush stdout;
  end;
  Collect.collect env;
  let env = { env with env = ref SMap.empty } in
  let pl = List.map (parameter env) fd.f_params in
  let ret = fresh() in
  let return = Tvar ret in
  let f = Tsum [Tfun (pl, return)] in
  GEnv.set_fun env (Ast.unwrap fd.f_name) f;
  List.iter (type_def env) (Graph.get_deps !(env.graph) (Ast.unwrap fd.f_name));
  Env.set env "$;return" return;
  stmtl env fd.f_body;
  make_return env ret;
  GEnv.set_fun env (Ast.unwrap fd.f_name) (Generalize.ty env ISet.empty f)

and make_return env r =
  match TEnv.get env r with
  | Tsum _ -> TEnv.set env r null
  | _ -> ()

and parameter env p =
  let pval =
    match p.p_type with
    | None -> Tvar (fresh())
    | Some (Hint x) ->
        (try get_hard_object env x
        with Not_found ->
          expr env (New (Id (wrap x), [])))
    | Some (HintArray) ->
        expr env (ConsArray [])
  in
  (match p.p_default with
  | None -> ()
  | Some e -> ignore (Type.unify env pval (expr env e))
  );
  Env.set env (Ast.unwrap p.p_name) pval;
  (Ast.unwrap p.p_name), pval

and class_id env x =
  if GEnv.mem_class env x then () else
  if not (Classes.mem env x) then () else
  class_def env (Classes.get env x)

and get_hard_object env c =
  let class_ = GEnv.get_class env c in
  let class_ = match class_ with Tsum [Tobject o] -> o | _ -> raise Not_found in
  SMap.find "__obj" class_

(* the type of an object is always in the field __obj of the class
   all the other fields are static methods/vars
*)
and get_object = function
  | Tsum [Tobject o] when SMap.mem "__obj" o->
      (match SMap.find "__obj" o with Tsum [Tobject o] -> o
      | _ -> SMap.empty)
  | _ -> SMap.empty

and get_class env x =
  class_id env x;
  GEnv.get_class env x

and class_def env c =
  if GEnv.mem_class env (Ast.unwrap c.c_name) then () else begin
  GEnv.set_class env (Ast.unwrap c.c_name) any;
  Collect.collect env;
  let parent, parent_name =
    match c.c_extends with
    | [x] -> get_class env x, x
    | _ -> Tvar (fresh()), "" in
  if env.verbose then begin
    incr env.count;
    Printf.printf "Typing class(%d/%d)[%d]: %s\n" !(env.count) !(env.total) env.depth (Ast.unwrap c.c_name); flush stdout;
  end;
  let env = { env with env = ref SMap.empty } in
  let class_ = match parent with Tsum [Tobject o] -> o | _ -> SMap.empty in
  let obj_parent = get_object parent in

  (* Adding traits *)
  let traits =
    List.map (fun (x, _) -> get_object (get_class env x)) c.c_traits in
  let obj_parent = List.fold_right (SMap.fold SMap.add) traits obj_parent in

  (* Declarations *)
  let is_enum = c.c_variables = [] && c.c_methods = [] in
  let ien, sen = List.fold_left (constant_enum is_enum c.c_name) (SSet.empty, SSet.empty) c.c_constants in
  let class_ = List.fold_left (constant is_enum env ien sen) class_ c.c_constants in
  let class_ = List.fold_left (class_vars true env) class_ c.c_variables in
  let class_ = List.fold_left (method_decl true env) class_ c.c_methods in

  let obj = List.fold_left (class_vars false env) obj_parent c.c_variables in
  let obj = List.fold_left (method_decl false env) obj c.c_methods in

  let this = Tsum [Tclosed (SSet.singleton (Ast.unwrap c.c_name), obj)] in
  let self = Type.unify env parent (Tsum [Tobject class_]) in

  GEnv.set_class env (Ast.unwrap c.c_name) self;
  Env.set env (special "self") self;
  Env.set env "$this" this;
  Env.set env (special "parent") (Tsum [Tobject obj_parent]);

  let class_ = List.fold_left (method_def true env) class_ c.c_methods in
  let obj = List.fold_left (method_def false env) obj c.c_methods in

  let obj = SMap.map (fun t -> Generalize.ty env ISet.empty t) obj in
  let privates = List.fold_left private_vars SSet.empty c.c_variables in
  let privates = List.fold_left private_methods privates c.c_methods in
  let obj = filter_privates privates obj in

  (* CHEATING :-) *)
  let obj = SMap.map (cheat_method env parent_name this) obj in

  let obj = Tsum [Tobject obj] in
(*  let obj = Generalize.ty env obj in
  let obj = Tlazy obj in *)
  let class_ = SMap.map (fun t -> Generalize.ty env ISet.empty t) class_ in
  let class_ = filter_privates privates class_ in
  let class_ = SMap.add "__obj" obj class_ in
  let class_ = (Tsum [Tobject class_]) in

(*  let class_ = Generalize.ty env class_ in
  make_globals env; *)
  GEnv.set_class env (Ast.unwrap c.c_name) class_
  end

and private_vars privates cv =
  if cv.cv_visibility = Private
  then SSet.add cv.cv_name privates
  else privates

and private_methods privates m =
  if m.m_visibility = Private
  then SSet.add (Ast.unwrap m.m_name) privates
  else privates

and filter_privates privates obj =
  SMap.fold (
  fun x t acc ->
    if SSet.mem x privates
    then acc
    else SMap.add x t acc
 ) obj SMap.empty

and constant_enum is_enum cname (ien, sen) (x, e) =
  match e with
  | Int _ -> SSet.add (Ast.unwrap cname^"::"^x) ien, sen
  | String _ -> ien, SSet.add (Ast.unwrap cname^"::"^x) sen
  | _ -> ien, sen

and constant is_enum env ien sen acc (x, e) =
  match e with
  | Int _ when is_enum -> SMap.add x (Tsum [Tienum ien]) acc
  | String _ when is_enum -> SMap.add x (Tsum [Tsenum sen]) acc
  | _ -> SMap.add x (expr env e) acc

and class_vars static env acc c =
  if static && not c.cv_static then acc else
  if not static && c.cv_static then acc else
  (cv_var static env) acc (c.cv_name, c.cv_value)

and cv_var static env acc (s, e) =
  let t = match e with None -> Tvar (fresh()) | Some x -> expr env x in
  let s = if static then s else String.sub s 1 (String.length s - 1) in
  SMap.add s t acc

and method_decl static env acc m =
  if m.m_static && not static then acc else
  if not m.m_static && static then acc else
  let pl = List.map (parameter env) m.m_params in
  let ret = fresh() in
  let f = afun pl (Tvar ret) in
  SMap.add (Ast.unwrap m.m_name) f acc

and method_def static env acc m =
  if m.m_static && not static then acc else
  if not m.m_static && static then acc else
  let env_cpy = !(env.env) in
  let pl = List.map (parameter env) m.m_params in
  let ret = fresh() in
  let return = Tvar ret in
  Env.set env "$;return" return;
  stmtl env m.m_body;
  make_return env ret;
  let f = afun pl (Env.get env "$;return") in
  let _ = Type.unify env (SMap.find (Ast.unwrap m.m_name) acc) f in
  env.env := env_cpy;
  SMap.add (Ast.unwrap m.m_name) f acc

and cheat_method env parent this m =
  match m with
  | Tsum [Tfun (x, Tsum [Tclosed (s, _)])] when SSet.mem parent s ->
      let v = Tvar (fresh()) in
      let v = Type.unify env v this in
      Tsum [Tfun (x, v)]
  | x -> x
