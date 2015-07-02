(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

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


module C = Piqi_common  
open C


(* idtable implemented as map: string -> 'a *)
module Idtable =
  struct
    module M = Map.Make(String)

    type 'a t = 'a M.t

    let empty = M.empty

    let add idtable name entry =
      M.add name entry idtable

    let find idtable name =
      M.find name idtable 

    let remove idtable name =
      M.remove name idtable

    let mem idtable name =
      M.mem name idtable 

    let fold f accu idtable =
      M.fold f idtable accu
  end


(* the map of loaded piqi modules: modname -> piqi *)
module Piqitable = Idtable
let loaded_map = ref Piqitable.empty


(* we use this function to prevent adding same module under several different
 * names *)
let normalize_name = U.underscores_to_dashes


(* find already loaded module by name *)
let find_piqi modname :T.piqi =
  Piqitable.find !loaded_map (normalize_name modname)


let try_find_piqi modname =
  try Some (find_piqi modname)
  with Not_found -> None


let add_piqi piqi =
  let modname = some_of piqi.P.modname in
  trace "piqi_db: caching piqi module \"%s\"\n" modname;

  let do_add_piqi modname piqi =
    loaded_map := Piqitable.add !loaded_map (normalize_name modname) piqi
  in
  (* check for name override/conflict *)
  (* XXX: prohibit override? *)
  match try_find_piqi modname with
    | Some prev_piqi when prev_piqi == piqi -> (* don't readd the same module *)
        ()
    | Some prev_piqi ->
        warning piqi ("redefinition of module " ^ U.quote modname);
        warning prev_piqi "previous definition is here";
        do_add_piqi modname piqi
    | None ->
        do_add_piqi modname piqi


let remove_piqi modname =
  loaded_map := Piqitable.remove !loaded_map (normalize_name modname)


let replace_piqi piqi =
  let modname = some_of piqi.P.modname in
  match try_find_piqi modname with
    | None -> ()
    | Some _ ->
        loaded_map := Piqitable.add !loaded_map (normalize_name modname) piqi


let find_local_typedef typedefs name =
  List.find (fun x -> name = typedef_name x) typedefs


(* To be set up later to Piqi.load_piqi_file; we do this since OCaml doesn't
 * support recursive toplevel modules *)
let piqi_loader :(?modname:string -> string -> T.piqi) option ref = ref None


let load_piqi_module modname =
  trace "piqi_db: loading module: %s\n" modname;
  trace_enter ();
  let fname = Piqi_file.find_piqi modname in (* can raise Not_found *)
  let load_piqi_file = some_of !piqi_loader in
  let piqi = load_piqi_file ~modname fname in
  trace_leave ();
  piqi


let find_load_piqi_typedefs ~auto_load_piqi modname =
  match modname with
    | None -> (* no modname means built-in type *)
        !C.builtin_typedefs
    | Some modname ->
        (* check if the module is already loaded, and return it right away *)
        let piqi =
          try find_piqi modname
          with Not_found when auto_load_piqi ->
            (* XXX: handle load errors *)
            load_piqi_module modname
        in
        piqi.P.resolved_typedef


let find_typedef ?(auto_load_piqi=true) name =
  (* XXX: check global type name before loading? *)
  trace "looking for type: %s\n" name;
  let modname, typename = Piqi_name.split_name name in
  let typedefs = find_load_piqi_typedefs modname ~auto_load_piqi in
  find_local_typedef typedefs typename


let find_piqtype name =
  let def = find_typedef name in
  (def: T.typedef :> T.piqtype)


let try_find_piqtype name =
  try
    let def = find_typedef name ~auto_load_piqi:false in
    Some (def: T.typedef :> T.piqtype)
  with
    Not_found -> None

