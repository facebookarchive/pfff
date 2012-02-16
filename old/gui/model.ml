(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Db = Database_php
module E = Entity_php

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = { 

  (* ------------------------ *)
  (* the data  *)
  (* ------------------------ *)
  db: Db.database;

  (* to access sgrep file or annotation file *)
  pfff_srcpath: Common.dirname;
  
  (* ------------------------ *)
  (* the query *)
  (* ------------------------ *)
  mutable basic_query: string;

  (* easier to use ref than mutable when want write generic code *)
  col1_name: string ref;
  col1: string list ref;

  col2_name: string ref;
  col2: string list ref;

  col3_name: string ref;
  col3: string list ref;

  col4_name: string ref;
  col4: string list ref;


  (* ----------------------- *)
  (* the rest *)
  (* ----------------------- *)

  (* todo? have a model.current_filename ? Right now I abuse current_id a
   * little for that, like when I open a file.
   *)
  mutable current_id : Db.id option;

  mutable playlist: Entities_php.idtree Common.undo_stack;

  mutable current_glimpse_query: string option;
(*
  mutable current_annot: Annotation.annotation option;

  mutable current_words_query: string list option;
  mutable current_smpl_query: Ast_cocci.rule_elem option;
  mutable current_smpl_query_protocol: 
    (Ast_cocci.rule_elem * Ast_cocci.rule_elem) option;
*)

  (* ------------------------ *)
  (* settings *)
  (* ------------------------ *)
  mutable calltree_prefs: Callgraph_php.calltree_preferences;

  mutable depth_caller: int;
  mutable depth_callee: int;

  (* todo: mutable highlighter_prefs: *)
  mutable show_type_error: bool;
  mutable show_local_global: bool;

  mutable abstract_proto_and_decl: bool;

}


let default_model db pfff_srcpath = {

  db = db;
  pfff_srcpath = pfff_srcpath;

  basic_query = "";

  col1_name = ref "";
  col1 = ref [];
  col2_name = ref "";
  col2 = ref [];
  col3_name = ref "";
  col3 = ref [];
  col4_name = ref "";
  col4 = ref [];

  current_id = None;
  playlist = Common.empty_undo_stack;
  calltree_prefs = Callgraph_php.default_calltree_preferences;

  current_glimpse_query = None;

(*

  annot_info = Annotations_list.empty_annotations_info;

  current_annot = None;

  current_words_query = None;
  current_smpl_query = None;
  current_smpl_query_protocol = None;

*)


  (* take care: if depth more than 4 then if have lots of callers then gtk can
   * be quite slow.
   *)
  depth_caller = 3;
  depth_callee = 2;

  show_type_error = false;
  show_local_global = true;

  abstract_proto_and_decl = true;
}

let init_model dir srcpath = 
  let db = Database_php.open_db dir in
  default_model db srcpath

let close_model model = 
  Database_php.close_db model.db;
  ()

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(*
let string_of_id id model =
  let name = model.db.Db.names#assoc id in
  Db.string_of_id_string name

let fullid_of_id id model =
  model.db.Db.fullid_of_id#assoc id 
*)

(* ---------------------------------------------------------------------- *)
let ids_of_string s model =

(*
  let db = model.db in
  if model.abstract_proto_and_decl then 
    let ids = Database.ids_with_kind2__of_string s db in

    let ids_filtered = ids +> Common.exclude (fun (id,kind2) -> 
      Entity.is_proto_or_decl2 kind2
    )
    in
    if null ids_filtered 
    then ids          +> List.map fst
    else ids_filtered +> List.map fst
  else 
*)
    model.db.Db.defs.Db.name_defs#assoc (s)



(* ---------------------------------------------------------------------- *)
(* todo have a model.current_filename ? *)
let current_file (model: model) =
  match model.current_id with
  | None -> failwith "no current file"
  | Some id -> 
      let fullid = model.db.Db.fullid_of_id#assoc id in
      fullid.E.file 

(*
(* ---------------------------------------------------------------------- *)

let sgrep_path model = 
  Filename.concat model.srcpath "data/sgrep"

let iso_file model = 
  Some (Filename.concat model.srcpath "config/isos/standard.iso")
  
let annots_of_id id model = 
  Annotations_database.annots_of_id id model.annot_info model.db

let annot_checking_info model = 

  let annots_of_id = 
    Annotations_database.mk_annots_of_id model.annot_info model.db 
  in
  { Annotation_checking.
      depth_caller = model.depth_caller;
      calltree_prefs = model.calltree_prefs;
      isofile = iso_file model;
      sgreppath = sgrep_path model;
      annots_of_id = annots_of_id;
  }

*)


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
