(*s: model_database_code.ml *)
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

module Flag = Flag_visual
module Db = Database_code

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(*s: readable_to_absolute_filename_under_root *)
(* People may run the visualizer on a subdir of what is mentionned in the
 * database code (e.g. subdir ~/www/flib of ~/www). The light_db
 * contains only readable paths (e.g. flib/foo.php); the reason for
 * those readable paths is that we want to reuse the light_db
 * and share it among multiple users which may have
 * different paths for their own software repo (e.g. ~/www4/).
 * 
 * When the user select an entity through the search box,
 * we will know the readable paths of the entity he is looking for
 * but we need a full path for refreshing the treemap.
 * We can not just concatenate the root with the readable paths which
 * in the example would lead to the path  ~/www/flib/flib/foo.php.
 * 
 * The goal of the function below is given a readable path like
 * flib/foo.php and a root like ~/www/flib  to recognize the common part
 * and return a valid fullpath like ~/www/flib/foo.php
 * 
 *)
let readable_to_absolute_filename_under_root ~root filename =

  (* the root may be a filename *)
  let root_dir = 
    if Common2.is_directory root then root
    else Filename.dirname root
  in

  let root_and_parents =
    Common2.inits_of_absolute_dir root_dir +> List.rev
  in
  try 
    root_and_parents +> Common2.return_when (fun dir ->
      let path = Filename.concat dir filename in
      if Sys.file_exists path
      then Some path
      else None
    )
  with Not_found ->
    failwith 
      (spf "can't find file %s with root = %s" filename root)
(*e: readable_to_absolute_filename_under_root *)

(*s: actual_root_of_db *)
let actual_root_of_db ~root db =
  let a_file = (db.Db.entities.(0)).Db.e_file in
  let absolute_file = 
    readable_to_absolute_filename_under_root root a_file in
  
  if absolute_file =~ ("\\(.*\\)/" ^ a_file)
  then Common.matched1 absolute_file
  else failwith (spf "Could not find actual_root of %s under %s: "
                    absolute_file root)
(*e: actual_root_of_db *)

(*****************************************************************************)
(* Entities info *)
(*****************************************************************************)

(*s: hentities() *)
(* We want to display very often used functions in bigger size font.
 * Enter database_code.ml which provides a language-independent database of
 * information on source code.
 * 
 * We compute the entities outside init_drawing because
 * init_drawing can be called multiple times (when we zoom in)
 * and we dont want the heavy entities computation to be 
 * repeated.
 *)
let hentities root db_opt = 
  let hentities = Hashtbl.create 1001 in

  db_opt +> Common.do_option (fun db ->

    let actual_root = actual_root_of_db ~root db in

      (* todo sanity check that db talks about files
       * in dirs_or_files ? Ensure same readable path.
       *)
      db.Db.entities +> Array.iter (fun e ->
        Hashtbl.add hentities
          e.Db.e_name
          {e with Db.e_file = 
              Filename.concat actual_root e.Db.e_file
          }
      );
  );
  hentities
(*e: hentities() *)

(*s: hfiles_and_top_entities() *)
(* used in the summary mixed mode *)
let hfiles_and_top_entities root db_opt =
  let hfiles = Hashtbl.create 1001 in

  db_opt +> Common.do_option (fun db ->
    let ksorted = 
      Db.build_top_k_sorted_entities_per_file ~k:5 db.Db.entities in
    let actual_root = actual_root_of_db ~root db in

    Hashtbl.iter (fun k v ->
      let k' = Filename.concat actual_root k in
      Hashtbl.add hfiles k' v
    ) ksorted
  );
  hfiles
(*e: hfiles_and_top_entities() *)

(*****************************************************************************)
(* Completion data *)
(*****************************************************************************)

(*s: all_entities *)
(* To get completion for functions/class/methods/files/directories.
 * 
 * We pass the root in addition to the db_opt because sometimes we 
 * don't have a db but we still want to provide completion for the 
 * dirs and files.
 * 
 * todo: what do do when the root of the db is not the root
 * of the treemap ?
 *)
let all_entities ~root files db_opt =
  match db_opt with
  | None -> 
      let db = Database_code.files_and_dirs_database_from_files ~root files in
      Database_code.files_and_dirs_and_sorted_entities_for_completion
        ~threshold_too_many_entities:!Flag.threshold_too_many_entities
        db

  | Some db ->
      let nb_entities = Array.length db.Db.entities in
      let nb_files = List.length db.Db.files in
      pr2 (spf "We got %d entities in %d files" nb_entities nb_files);

      (* the db passed might be just about .ml files but we could be
       * called on a directory with non .ml files that we would
       * still want to quicky jump too hence the need to include
       * other regular files and dirs
       *)
      let db2 = Database_code.files_and_dirs_database_from_files ~root files in
      let db = Database_code.merge_databases db db2 in

      Database_code.files_and_dirs_and_sorted_entities_for_completion
        ~threshold_too_many_entities:!Flag.threshold_too_many_entities
        db
(*e: all_entities *)

(*e: model_database_code.ml *)
