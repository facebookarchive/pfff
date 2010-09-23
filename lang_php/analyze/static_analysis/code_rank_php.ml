(*s: code_rank_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common

module Flag = Flag_analyze_php
module Db   = Database_php
module EC   = Entity_php
module CG   = Callgraph_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * reference:
 *   "CodeRank: A New Family of Software Metrics"
 *    B. Neate, W. Irwin,  N. Churcher
 * 
 * Thx to sebastien bergmann for pointing out this paper.
 * 
 * How it compares to naive metrics which is counting the number of callers ?
 * Do we find more important functions ? Normally if a function is 
 * not very often called, but called by an important function, coderank
 * will find it, but our naive metrics will not. Do we have such 
 * example of important but small function ?
 * 
 * 
 *)
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type code_ranks = {
  function_ranks: (Database_php.id, float) Oassoc.oassoc;
}
let empty_code_ranks () = { 
    function_ranks = new Oassoch.oassoch [];
  }

(* code id. Can be just equal to Database_php.id, which may generate 
 * sparse tables, or something else that requires a mapping from it 
 * to a Database_php.id
 *)
type cid = int

(* big, indexed by code id *)
type ranks = float array

  (* fast enough ? why not float array ? because later we may want to
   * and cr = {
   *  mutable score: float;
   * mutable cid: cid;
   * }
   *)


type equation_right = (cid * float (* dividing factor *))  list

(* big, indexed by code id *)
type equations = equation_right array

(* side effect on ranks *)
let eval_equation ranks d eq = 
  let contribs = 
    (eq +> List.fold_left (fun acc (cid,  diviser) -> 
      (* bugfix: forgot to add acc :) *)
      acc +. ranks.(cid) /. diviser
    ) 0.0)
  in
  let newv = (1.0 -. d) +. d *. contribs
  in
  newv

(*****************************************************************************)
(* Helper *)
(*****************************************************************************)

let (initial_value: Database_php.id list -> int -> ranks) = fun ids maxid ->
  let (arr: ranks) = Array.make (maxid + 1) 0.1 in
  arr

let (equations: Database_php.id list -> int -> Database_php.database -> equations) =
 fun ids maxid db ->
  let (eqs: equations) = Array.make (maxid + 1) [] in

  ids +> List.iter (fun id -> 
    let (EC.Id this_cid) = id in

    (* note that the callers may be toplevel statement *)
    let callers = Db.callers_of_id id db in
    let eq_right = 
      callers +> Common.map_filter (fun callerinfo ->
        let callerid = CG.id_of_callerinfo callerinfo in
        
        let (EC.Id cid) = callerid in
        (* ignore for now caller which are not functions *) 

        if not (Db.is_function_id callerid db) 
           || callerid = id (* dont want recursive calls to messup data *)
        then None
        else 
          let callees_of_caller = 
            Db.callees_of_id callerid db in
          
          let nbcallees = List.length callees_of_caller in
          Some (cid, float_of_int nbcallees)
      )
    in
    eqs.(this_cid) <- eq_right;
  );
  eqs

let (fixpoint: ranks -> equations -> ranks) = fun ranks equations ->

  let changed = ref true in
  let depth_limit = 500 in
  let step = ref 0 in
  pr2 ("computing code ranks fixpoint");

  while !changed && !step < depth_limit do
    changed := false;
    incr step;
    pr2 (spf "step = %d" !step);

    for i = 0 to Array.length ranks - 1 do
      let oldv = ranks.(i) in
      let newv = eval_equation ranks 0.85  equations.(i) in
      if oldv <> newv 
      then changed := true;
      ranks.(i) <- newv;
    done
  done;
  ranks

let (ranks_to_code_ranks: Database_php.id list -> ranks -> code_ranks) = 
 fun ids ranks ->
  let cr = empty_code_ranks () in
  ids +> List.iter (fun (EC.Id cid) ->
    cr.function_ranks#add2 (EC.Id cid, ranks.(cid));
  );
  cr

(*****************************************************************************)
(* Builder *)
(*****************************************************************************)

let build_code_ranks db = 
  
  (* let's focus on functions first *)
  let funcids = Db.functions_in_db db in
  
  let allids = ref [] in
  let maxid = ref 0 in

  funcids +> List.iter (fun (s, ids) ->
    pr2 s;
    ids +> List.iter (fun (EC.Id id) ->
      Common.push2 (EC.Id id) allids;
      if id > !maxid then maxid := id;
    );
  );

  pr2 (spf "maxid = %d, nb ids = %d" !maxid (List.length !allids));

  let ranks = initial_value !allids !maxid in 
  let eqs = equations !allids !maxid db in
  let ranks = fixpoint ranks eqs in
  ranks_to_code_ranks !allids ranks
  


let build_naive_caller_ranks db = 
  let funcids = Db.functions_in_db db in
  let cr = empty_code_ranks () in
  funcids +> List.iter (fun (s, ids) ->
    ids +> List.iter (fun id ->
      let nbcallers = Db.callers_of_id id db in
      cr.function_ranks#add2 (id, float_of_int (List.length nbcallers));
    )
  );
  cr
    

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

(* toy example in paper is in tests/code_rank 
*)

(*e: code_rank_php.ml *)
