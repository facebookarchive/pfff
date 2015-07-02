(* object location DB *)
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


type loc = string * int * int (* file, line, column *)


(* whether to verify the correctness of location information *)
let check = ref false

(* whether to print debug information *)
let trace = ref false

(* whether to crash with Piqloc_not_found if can't find the location; this is
 * useful for obtaining the exact stacktrace where the location DB consistency
 * is violated *)
let crash_on_error = ref false

exception Piqloc_not_found


(* pause & resume storing location references; NOTE: it is paused from the
 * beginning because there's some boot actions in piqi_piqi.ml would generate
 * skewed location counters if we didn't pause it *)
let is_paused = ref 1 (* > 0 means paused; pause() calls can be nested *)

(* TODO: provide a more reliable way to resume in case of exceptions rather than
 * calling resume () manually; we could use the same apprach as in
 * Piqi_util.with_bool *)
let pause () =
  incr is_paused;
  if !trace
  then Printf.eprintf "Piqloc.pause: count = %d\n" !is_paused

let resume () =
  decr is_paused;
  if !trace
  then Printf.eprintf "Piqloc.resume: count = %d\n" !is_paused


(* input and output wire location counters *)
let icount = ref 0
let ocount = ref 0


(* internal locator structure: location can be represented by either location
 * itself or by reference to an object which is registered in the location DB *)
type t = Loc of loc | Ref of Obj.t


let db :(Obj.t * t) list ref = ref []

(* append-only part of the DB that is filled by preserve() can't be discarded by
 * reset() *)
let preserved_db :(Obj.t * t) list ref = ref []
let preserved_count = ref 0


(* similar to List.assq but also return the tail of the list after the matched
 * element *)
let list_assq_return_tail k l =
  let rec aux = function
    | [] -> raise Not_found
    | (k', v)::t when k' == k -> v, t
    | _::t ->
        aux t
  in
  aux l


(* recursively dereference to find the location *)
let find_in_db x db ~trace =
  let rec aux k l =
    let entry, t = list_assq_return_tail k l in
    match entry with
      | Loc loc ->
          loc
      | Ref new_key ->
          if trace then Printf.eprintf "Piqloc.find: %d\n" (Obj.magic x);
          aux new_key t
  in
  let key = Obj.repr x in
  aux key db


let find ?(trace=false) x =
  try find_in_db x !db ~trace
  with Not_found ->
    find_in_db x !preserved_db ~trace


let lastloc = ref ("undefined", 0, 0) (* whatever -- initial value *)


let setloc loc =
  lastloc := loc


let add x =
  if !trace
  then (
    let f, i, j = !lastloc in
    Printf.eprintf "Piqloc.add: %d at (%s, %d, %d)\n" (Obj.magic x) f i j;
  );
  db := (Obj.repr x, Loc !lastloc)::!db


let addloc loc x = 
  setloc loc; add x


(* add object within the current location and return the object *)
let addret x =
  add x; x


(* add object within the specified location and return the object *)
let addlocret loc x =
  addloc loc x; x


(* Discard location information. This allows GC to reclaim memory used by data
 * objects that are not referenced from anywhere else other than from location
 * db *)
let reset () =
  db := [];
  icount := !preserved_count;
  ocount := !preserved_count;
  ()


(* Preserve location information by copying the contents of db to preserved_db
 * so that exising location info won't be discarded by subsequent reset() calls.
 *)
let preserve () =
  preserved_db := !db @ !preserved_db;
  preserved_count := max !icount !ocount;
  if !trace
  then Printf.eprintf "Piqloc.preserve: preserved_count = %d\n" !preserved_count;
  reset ()


(* check if location for the object exists in the loc database *)
let do_check_loc ?(trace=false) x =
    ignore (find x ~trace)


let do_add_fake_loc ?(label="") x =
  let fake_loc = ("fake" ^ label, 0, 0) in
  db := (Obj.repr x, Loc fake_loc)::!db;

  let f, i, j = fake_loc in
  if !trace
  then Printf.eprintf "Piqloc.do_add_fake_loc: %d at %d (%s, %d, %d)\n" (Obj.magic x) (Obj.magic fake_loc) f i j


let add_fake_loc ?(label="") x =
  if !trace || !check
  then (
    try
      do_check_loc x ~trace:false;
      Printf.eprintf "Warning: internal error:\n";
      Printf.eprintf "Piqloc.add_fake_loc REAL LOC IS ALREADY PRESENT: %d:\n" (Obj.magic x);
      (* now, printing the actual search sequence *)
      Printf.eprintf "--\n";
      do_check_loc x ~trace:true;
      Printf.eprintf "--\n";
      if !crash_on_error
      then raise Piqloc_not_found;
    with Not_found ->
      do_add_fake_loc x ~label
  )


let check_loc x =
  if !trace || !check
  then (
    try
      do_check_loc x
    with Not_found -> (
      Printf.eprintf "Warning: internal error:\n";
      Printf.eprintf "Piqloc.check_loc NOT FOUND: %d\n" (Obj.magic x);
      (* now, printing the actual search sequence *)
      Printf.eprintf "--\n";
      try do_check_loc x ~trace:true with Not_found -> ();
      Printf.eprintf "--\n";
      if !crash_on_error
      then raise Piqloc_not_found
    )
  )


let is_paused_once = ref false

let pause_once () =
  if !is_paused = 0
  then is_paused_once := true


let addref dst src =
  if Obj.repr src == Obj.repr dst || !is_paused > 0 || !is_paused_once
  then (
    (* nothing to do except for unpausing when pause_once () was requested *)
    if !is_paused_once then is_paused_once := false
  )
  else (
    if !trace || !check
    then (
      try
        let loc = find dst ~trace:false in
        if !trace
        then (
          let f, i, j = loc in
          Printf.eprintf "Piqloc.addref: %d at %d (%s, %d, %d)\n" (Obj.magic src) (Obj.magic dst) f i j
        );
        (* move the actual location record of the dst to the head of the list --
         * this is an optimization that helps subsequent find () calls to work
         * faster at a cost of using more memory *)
        db := (Obj.repr dst, Loc loc)::!db
      with
        | Not_found when Obj.is_int (Obj.repr dst) -> (* is integer reference? *)
            (* this can be a legitimate case; for example, during
             * mlobj_to_piqobj conversion, field mode (e.g. `required) is an
             * unboxed value that doesn't generate a reference
             *
             * need to a add a fake reference here so that it doesn't cause more
             * errors during subsequent conversions *)
            do_add_fake_loc dst ~label:"_addref_not_found_int"
        | Not_found -> (
            Printf.eprintf "Warning: internal error:\n";
            Printf.eprintf "Piqloc.addref: %d at %d -- NOT FOUND\n" (Obj.magic src) (Obj.magic dst);
            (* now, printing the actual search sequence *)
            Printf.eprintf "--\n";
            try do_check_loc dst ~trace:true with Not_found -> ();
            Printf.eprintf "--\n";
            if !crash_on_error
            then raise Piqloc_not_found;
            (* adding a fake reference here to stop this error from reoccurring
             * later *)
            do_add_fake_loc dst ~label:"_addref_not_found"
        )
    );

    (* now, adding the actual record to the location database *)
    db := (Obj.repr src, Ref (Obj.repr dst))::!db;
  )


let addrefret dst src = (* add reference and return *)
  addref dst src; src


(* store resulting object -> source object correspondent in the location DB *)
let reference f x =
  let res = f x in
  addrefret x res


let next_icount () =
  let res = !icount in
  if !is_paused = 0 then incr icount;
  res

let next_ocount () =
  let res = !ocount in
  if !is_paused = 0 then incr ocount;
  res

