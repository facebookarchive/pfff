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

(*
 * unique integer key -> untyped magic object store
 *
 * TODO: values need to be garbage-collected at certain times; we need to handle
 * this aspect like in piqloc.ml
 *)


let next_key = ref 0

let get_next_key () =
  let key = !next_key in
  incr next_key;
  key
  

(* integer -> Obj.t map *)
module Int =
  struct
        type t = int
        let compare = Pervasives.compare
  end

module M = Map.Make(Int)

let values = ref M.empty


let put value =
  let key = get_next_key () in
  values := M.add key (Obj.repr value) !values;
  key


let get key =
  let value = M.find key !values in
  Obj.obj value

