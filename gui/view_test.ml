open Common

open Model

module Db = Database_php

(* Provides a way to automate a few things, to command-line-ify the GUI *)

(* estet? a little duplication with code in view_dialogs ? factorize ? *)
let do_command s model = 
  match s with
  | s when s =~ "function:\\(.*\\)" -> 
      let s = matched1 s in
      let ids = model.db.Db.defs.Db.name_defs#assoc (s) in
      Controller.refresh_source_and_id (Some (List.hd ids)) model;
      


  | s when s =~ "file:\\(.*\\)" -> 
      let s = matched1 s in
(*      
      let ids = model.db.Db.file_to_ids#assoc s in
      let tree = Db.tree_of_ids ~sort:false ids model.db in
      Controller.refresh_playlist tree model;
      Controller.refresh_source_and_id (Some (List.hd ids)) model;
*)
      ()


  | s when s =~ "caller:\\(.*\\)" -> 
      let id_str = matched1 s in
(*
      let ids = Model.ids_of_string id_str model in
      Controller.refresh_source_and_id (Some (List.hd ids)) model;
      View_dialogs.build_callers model;
*)
      ()
  | s when s =~ "callee:\\(.*\\)" -> 
      let id_str = matched1 s in
(*
      let ids = Model.ids_of_string id_str model in
      Controller.refresh_source_and_id (Some (List.hd ids)) model;
      View_dialogs.build_callees model;
*)
      ()

      
  | _ -> failwith "no valid command argument"
