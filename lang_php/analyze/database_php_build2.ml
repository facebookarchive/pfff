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

open Database_php

module Db = Database_php

module T = Type_php

module Typing = Typing_php
module TAC  = Type_annotater_php



(*****************************************************************************)
(* Helpers ast and tokens *)
(*****************************************************************************)

(*****************************************************************************)
(* Build database intermediate steps *)
(*****************************************************************************)


(* ---------------------------------------------------------------------- *)
(* stepX:
 *  - TODO type inference using global information build via index_db2.
 *    Very useful for giving back type information about variables
 *    to the programmer. Also of course usuful to find bugs (type errors).
 *    Can be used to improve later the function alias pointer 
 *    analysis too.
 *    TODO add type info in toplevel asts, and also nested asts ?
 * 
 * Use database_info_to_typing_environment defined above.
 *)

let index_todo_typing () =
  let _env_typing = ref (Hashtbl.create 101)
    (* database_info_to_typing_environment db *)
  in

    (* let ast = Typing.annotate_toplevel env_typing ast in *)
  raise Todo

    (*
    let was_modified = 
      Typing.re_annotate_missing_info env_typing ast
    in
    if was_modified (* && false (* to test *) *)
    then db.objects#add2 (id, ast);
    *)



(* Before I was using for the kinds table just a simple (id, id_kind)
 * Oassoc.oassoc. But in index_db1 I sometimes overwrite previous
 * entries as some id may contain both a variable and struct def. So
 * it was buggy. But magically some functions such as
 * database_info_to_typing_c_environment was still working correctly
 * even when globals was defined with their struct definition
 * together. Why ? because in such case the type of the global was
 * taken from its ast and so was the complete struct def. I didn't
 * normalize the type.
 * 
 * opti? use cache in environment ? to not search again and again
 * in database for the type of some globals or fields ?
 * 
 *)

(*
let database_info_to_typing_environment db = { 
  Typing.global = (fun s -> Database_query.type_of_ident s db);
  );
}
*)



(* ---------------------------------------------------------------------- *)

(* Funcall to id cache. And also threshold cache for types.
 * The dumpfile can be generated for example by fb_phpunit_wrap or
 * facebook/check_module/testModule.
 * See also Test_analyze_php.test_type_xdebug_php.
 *) 
let index_db_xdebug2 db dumpfile = 

  let h_id = Hashtbl.create 100000 in
  let _h_count = Common.hash_with_default (fun () -> 0) in

  let h_pbs_not_found   = Common.hash_with_default (fun () -> 0) in
  let h_pbs_multi_found = Common.hash_with_default (fun () -> 0) in

  let nb_not_found = ref 0 in

  dumpfile +> Xdebug.iter_dumpfile (fun call ->
    let caller = call.Xdebug.f_call in
    let params = call.Xdebug.f_params in
    let str = Callgraph_php.s_of_kind_call caller in
    
    let tparams = params +> List.map Typing_trivial_php.type_of_expr in

    try 
      let id = Db.id_of_kind_call caller db in
        
      let ft = 
        [T.Function (tparams +> List.map (fun t -> Some t), 
                     [T.Unknown])
        ]
      in
      if not (Hashtbl.mem h_id id)
        then begin
          Hashtbl.add h_id id true;
          db.defs.id_type#add2 (id, ft);
        end;

        
        ()
      with 
      | Not_found -> 
          pr2_once ("Entity not found: " ^ str);
          incr nb_not_found;
          h_pbs_not_found#update str (Common.add1)
      | Multi_found -> 
          pr2_once ("Entity multi found: " ^ str);
          incr nb_not_found;
          h_pbs_multi_found#update str (Common.add1)
  );
  pr_xxxxxxxxxxxxxxxxx ();
  pr "Entities Not found";
  pr_xxxxxxxxxxxxxxxxx ();
  h_pbs_not_found#to_list +> Common.sort_by_val_highfirst +> 
    List.iter (fun (s, count) -> pr (spf "%20s : %d" s count));

  pr_xxxxxxxxxxxxxxxxx ();
  pr "Entities Multi found";
  pr_xxxxxxxxxxxxxxxxx ();
  h_pbs_multi_found#to_list +> Common.sort_by_val_highfirst +> 
    List.iter (fun (s, count) -> pr (spf "%20s : %d" s count));


  pr_xxxxxxxxxxxxxxxxx ();
  pr (spf "nb entity not found: %d" !nb_not_found);
  ()

let index_db_xdebug a b = 
  Common.profile_code "Db.index_db_xdebug" (fun () -> index_db_xdebug2 a b)

(* ---------------------------------------------------------------------- *)
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
  let msg = "ANALYZE_INCLUDE_REQUIRE" in
  Database_php_build.iter_files db (fun ((file, topids), i, total) ->
    Common.pr2 (spf "%s: %s %d/%d " msg file i total);

    let program = topids +> List.map (fun id -> db.defs.toplevels#assoc id) in
    let increq = Include_require_php.top_increq_of_program program in
    
    let included_files = 
      increq |> Common.map_filter (fun (_kind, tok, inc_expr) ->
      let dir = Filename.dirname file in
      let path_opt = 
        Include_require_php.resolve_path (env, dir) inc_expr
      in
      match path_opt with
      | Some path -> Some path
      | None ->
          pr ("Bad include, can not statically determine the path");
          Lib_parsing_php.print_match ~format:Lib_parsing_php.Emacs [tok];
          None
      )
    in
    let additional = hook_additional_includes file program in
    
    (* some self check that we use the same path format *)
    additional |> List.iter (fun file ->
      try 
        let _ = absolute_to_readable_filename file db in
        ()
      with exn ->
        failwith (spf "The file %s in not in the project %s, exn = %s"
                     file (path_of_project db.project) (Common.exn_to_s exn))
    );

    let included_files = additional ++ included_files in

    db.uses.includees_of_file#add2 (file, included_files);
    included_files |> List.iter (fun included ->
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

  "-index_db_xdebug", "   <db> <dumpfile>", 
    Common.mk_action_2_arg (fun dbname dumpfile -> 
      with_db ~metapath:dbname (fun db -> index_db_xdebug db dumpfile));

  "-index_db_includes_requires", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname (fun db -> index_db_includes_requires2 None db)
    );

]
