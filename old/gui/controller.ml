(* Yoann Padioleau
 *
 * Copyright (C) 2009 Facebook
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

(* estet?  type 'args action = ('args -> unit) ref *)

(*****************************************************************************)
(* Basic *)
(*****************************************************************************)

let (_refresh_all_func : (Model.model -> unit) list ref) = 
  ref []
let refresh_all model = 
  begin
    pr2 "refresh";
    !_refresh_all_func +> List.iter (fun f -> f model);
  end

let (_before_quit_all_func : (Model.model -> unit) list ref) = 
  ref []
let before_quit_all model = 
  begin 
    pr2 "quitting";
    !_before_quit_all_func +> List.iter (fun f ->  f model);
  end


(*****************************************************************************)
(* semi generic *)
(*****************************************************************************)

let (expand_all: (unit -> unit) ref) = 
  ref (fun () -> failwith "no expand_all")
let (expand_level: (int -> unit) ref) = 
  ref (fun _ -> failwith "no expand_level")

(*****************************************************************************)
(* Specific *)
(*****************************************************************************)
let (_refresh_source : (Database_php.id option -> Model.model -> unit) ref) = 
  ref (fun m id -> failwith "refresh source controller not set")

let (_refresh_info_id : (Database_php.id -> Model.model -> unit) ref) = 
  ref (fun m id -> failwith "refresh info id not set")



let refresh_source_and_id optid model =
  model.Model.current_id <- optid;
(* XXX
  (match optid with 
  | Some id -> !_refresh_info_id id model;
  | None -> ()
  );
*)
  !_refresh_source optid model


(* ------------------------------------------------------------------------- *)
let (_refresh_playlist : (Callgraph_php.calltree -> Model.model -> unit) ref) = 
  ref (fun m id -> failwith "refresh playlist controller not set")
let refresh_playlist ids model =
  model.Model.playlist <- Common.push_undo ids model.Model.playlist;
  !_refresh_playlist ids model



(* ------------------------------------------------------------------------- *)
(*
let (_refresh_annotlist : 
  (Annotations_list.annotations -> Model.model -> unit) ref) = 
  ref (fun m id -> failwith "refresh annotlist controller not set")
let refresh_annotlist annots model =
  !_refresh_annotlist annots model

(* TODO refresh_annotlist_with_match_result *)

*)

(* ------------------------------------------------------------------------- *)
let (type_info_at_point: (unit -> unit) ref) = 
  ref (fun () -> failwith "no type info at point def")
let (string_at_point: (unit -> string) ref) = 
  ref (fun () -> failwith "no string at point def")
