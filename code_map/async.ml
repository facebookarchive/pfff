(*s: async.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(*s: type async *)
type 'a t = {
  m: Mutex.t; 
  c: Condition.t;
  v: 'a option ref;
  }
(*e: type async *)

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

(*s: async functions *)
let async_make () = {
  m = Mutex.create (); 
  c = Condition.create ();
  v = ref None;
}

let with_lock f l =
  Mutex.lock l;
  try 
    let x = f () in 
    Mutex.unlock l; 
    x
  with e -> 
    Mutex.unlock l; 
    raise e

let async_get a = 
  let rec go a =
    match !(a.v) with
    | None -> 
        pr2 "not yet computed";
        Condition.wait a.c a.m; 
        go a
    | Some v -> v
  in
  with_lock (fun () -> go a) a.m

let async_set v a = 
  with_lock (fun () ->
    a.v := Some v;
    Condition.signal a.c;
  ) a.m

let async_ready a = 
  (* actually I don't think you need the lock *)
  with_lock (fun () ->
    match !(a.v) with
    | Some _ -> true
    | None -> false
  ) a.m

let async_get_opt a =
  if async_ready a
  then Some (async_get a)
  else None

(*e: async functions *)

(*e: async.ml *)
