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

open Ast_php
open Database_php
module Db = Database_php
module DbH = Database_php_build_helpers

open Database_php_build_helpers

(*****************************************************************************)
(* Helpers ast and tokens *)
(*****************************************************************************)

(*****************************************************************************)
(* Build database intermediate steps *)
(*****************************************************************************)

(* Why put this here ? why not put it directly in database_php_build.ml ?
 *  
 * Because this function require an environment to correctly resolve
 * the path filenames which for some projects may not be needed. 
 * Many of our global analysis like the caller/callees get only few
 * ambiguities and so do not require a precise scope analysis.
 * 
 * This would also force the user to give each time an environment 
 * when he wants to analyze any code. 
 * 
 * The hook is to allow for instance to analyze codebase that have
 * defined a module system on top of the existing include/require
 * (e.g. facebook require_module() statements).
 *)
let index_db_includes_requires2 
 ?(hook_additional_includes = (fun file prog -> []))
 env_opt db =
  let env = 
    match env_opt with
    | Some env -> env
    | None ->
        let php_root = Db.path_of_project db.project in
        Env_php.mk_env ~php_root
  in
  pr2 "PHASE EXTRA 1: analyze include/require";
  DbH.iter_files db (fun (file, topids) ->

    let program = topids +> List.map (fun id -> db.defs.toplevels#assoc id) in
    let increq = Include_require_php.top_increq_of_program program in
    
    let included_files = 
      increq +> Common.map_filter (fun (_kind, tok, inc_expr) ->
      let dir = Filename.dirname file in
      let path_opt = 
        Include_require_php.resolve_path (env, dir) inc_expr
      in
      match path_opt with
      | Some path -> Some path
      | None ->
          pr2 ("Bad include, can not statically determine the path");
          Lib_matcher.print_match ~format:Lib_matcher.Emacs [tok];
          None
      )
    in
    let additional = hook_additional_includes file program in
    
    (* some self check that we use the same path format *)
    additional +> List.iter (fun file ->
      try 
        let _ = absolute_to_readable_filename file db in
        ()
      with exn ->
        failwith (spf "The file %s in not in the project %s, exn = %s"
                     file (path_of_project db.project) (Common.exn_to_s exn))
    );

    let included_files = additional ++ included_files in

    db.uses.includees_of_file#add2 (file, included_files);
    included_files +> List.iter (fun included ->
      db.uses.includers_of_file#apply_with_default included
        (fun old -> file::old) (fun() -> []) +> ignore
    );
  )

let index_db_includes_requires ?hook_additional_includes a b =
  Common.profile_code "Db.index_db_includes_requires" (fun () ->
    index_db_includes_requires2 ?hook_additional_includes a b)

(* TODO: having more precise scope information about what is included and
 * what is not, we could refine some of our analysis to remove some
 * ambiguities.
 *)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-index_db_includes_requires", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname (fun db -> index_db_includes_requires2 None db)
    );
]
