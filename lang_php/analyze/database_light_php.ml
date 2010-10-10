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

module Ast = Ast_php

module Db = Database_code

module DbPHP = Database_php

module HC = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_entity ~root id nb_users good_example_ids properties db =
  let l = DbPHP.line_of_id id db in
  let c = DbPHP.col_of_id id db in
  let name = DbPHP.name_of_id id db in
  let file = DbPHP.filename_of_id id db in
  let kind = DbPHP.kind_of_id id db in
  
  (* so will see in completion popup the possible method
   * for a class when start typing the class
   *)
  let fullname = DbPHP.complete_name_of_id id db in

  { Database_code.
    e_name = name;
    e_fullname = 
      if fullname <> name then fullname else "";

    e_file = Common.filename_without_leading_path root file;
    e_pos = { Common.l = l; Common.c = c };
    e_kind = 
      (match kind  with
      | Entity_php.Function -> Db.Function
      | Entity_php.Class -> Db.Class
      | Entity_php.Method -> Db.Method
      | Entity_php.StaticMethod -> Db.StaticMethod
      | Entity_php.Interface -> Db.Class (* todo ? interface ? *)

      | _ -> raise Impossible
      );
    e_number_external_users = nb_users;
    e_good_examples_of_use = good_example_ids; 
    e_properties = properties;
  }      

let exclude_ids_same_file ids idfile db =
  ids +> Common.exclude (fun id2 ->
    let idfile2 = DbPHP.filename_of_id id2 db in
    (* less: could filter when only called from another dir ? *)
    idfile = idfile2
  )


let is_pleac_file file = 
  let file = Common.lowercase file in
  file =~ ".*pleac*"

(* todo? should perhaps be a property in database_php.ml, so
 * don't have to put facebook specific stuff here ?
 *)
let is_test_file file =
  let file = Common.lowercase file in
  (file =~ ".*__tests__.*") ||
  (file =~ ".*tests/push-blocking") ||
  (file =~ ".*tests/push-monitoring") ||
  (file =~ ".*scripts/unittest/tests") ||
  false


let is_test_or_pleac_file file = 
  is_test_file file || is_pleac_file file


(* coupling: with phase 1 where we collect entities *)
let is_id_with_entity id db =
  match DbPHP.kind_of_id id db with
  | Entity_php.Function
  | Entity_php.Method
  | Entity_php.StaticMethod  
  | Entity_php.Class 
  | Entity_php.Interface
    -> 
      true
  | Entity_php.ClassConstant
  | Entity_php.ClassVariable
  | Entity_php.XhpDecl
  | Entity_php.IdMisc
  | Entity_php.StmtList 
    -> 
      false


(* the number of callers in the "example_of_use" should
 * be small to be a good example of use
 * notes: those are phpdb ids.
 *)
let rank_and_filter_examples_of_use ids db =
  ids +> List.map (fun id ->
    let file = DbPHP.filename_of_id id db in
    
    let nb_callees = List.length (DbPHP.callees_of_id id db) in

    (* Low means better; so prefer small size and pleac files.
     * TODO: when have test files from PHP manual, score ?
     *  could do: pleac first, then php manual example, then flib test,
     *  then other test files
     *)
    let score = 
      nb_callees / (if is_pleac_file file then 4 else 1) in
    score, id
  )
  +> Common.sort_by_key_lowfirst 
  +> List.map snd
  
let good_examples_of_use external_callers db = 
  let candidates = 
    external_callers +> List.filter (fun id ->
      let file = DbPHP.filename_of_id id db in
      is_test_or_pleac_file file &&
      (* can have toplevel statements as callers which later
       * will not have a id_db that we can refer to.
       *)
      is_id_with_entity id db 
    )
  in
  let candidates = rank_and_filter_examples_of_use candidates db in

  (* don't want to increase the size of the db so just take a few one *)
  Common.take_safe 3 candidates +> List.map (fun (Entity_php.Id i) -> i)
  
(*****************************************************************************)
(* Properties *)
(*****************************************************************************)

(* For function can look in AST if contains dynamic calls.
 * Also look for parameters passed by ref.
 *)



let properties_of_function_or_method id db =
  let id_ast = DbPHP.ast_of_id id db in

  let ps = ref [] in

  let params, body = 
    match id_ast with
    | Ast_entity_php.Function def ->
        (* TODO *)
        def.Ast.f_params +> Ast.unparen +> Ast.uncomma, 
        def.Ast.f_body +> Ast.unbrace
    | Ast_entity_php.Method def ->
        def.Ast.m_params +> Ast.unparen +> Ast.uncomma, 
        (match def.Ast.m_body with
        | Ast.AbstractMethod _ -> []
        | Ast.MethodBody body -> body +> Ast.unbrace
        )
    | _ -> 
        failwith "was expecting a Function or Method entity"
  in
  (* passed by ref *)
  params +> Common.index_list_0 +> List.iter (fun (p, i) ->
    if p.Ast.p_ref <> None
    then Common.push2 (Db.TakeArgNByRef i) ps;
  );

  (* call dynamic stuff 
   * todo: should be done via bottomup analysis and using dataflow information
   *  so even if people define lots of wrappers around the builtin
   *  dynamic function, then it does not matter.
   *)
  let calls = Lib_parsing_php.get_all_funcalls_in_body body in
  let dyncalls = Lib_parsing_php.get_all_funcvars_in_body body in

  if not (null dyncalls) ||
     calls +> List.exists (fun s -> 
       Hashtbl.mem Env_php.hdynamic_call_wrappers s)
  then Common.push2 (Db.ContainDynamicCall) ps;

  (* dead function *)

  (* todo: reflection (also recursive), use global (also recursive),
   *  dead statements (may need to know exit-like functions),
   *  code coverage.
   *)
     

  !ps

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let database_code_from_php_database ?(verbose=false) db =

  let root = DbPHP.path_of_project_in_database db in
  pr2 (spf "generating PHP db_light with root = %s" root);

  let files = DbPHP.all_files db in
  let dirs = files +> List.map Filename.dirname +> Common.uniq_eff in

  (* phase 1: collecting entities and basic information *)
  let entities = 
    db.DbPHP.defs.DbPHP.id_kind#tolist +> Common.map_filter (fun (id, id_kind)->
      match id_kind with
      | Entity_php.Function
      | Entity_php.Method
      | Entity_php.StaticMethod  
        ->
          let callers = DbPHP.callers_of_id id db 
            +> List.map Callgraph_php.id_of_callerinfo in
          let idfile = DbPHP.filename_of_id id db in
          let external_callers = exclude_ids_same_file callers idfile db in

          (* note that this will return DbPHP ids, not Db ids. Later
           * once we got the full array, we will translate those DbPHP ids
           * into Db ids
           * 
           * Quite simple algorithm to find good examples of use:
           *  we just look at callers, filter the one in test file, then
           *  for each caller, look at number of callees, and take
           *  the one with best ratio.
           * 
           * coupling: note that the e_number_external_users is not that 
           * good for methods because of the approximation we currently do
           * in our class/method analysis. That's why we have another
           * phase later to adjust the method callers count.
           *)
          let good_ex_ids = good_examples_of_use external_callers db in
          let properties = properties_of_function_or_method id db in

          Some (id, mk_entity 
                   ~root id 
                   (List.length external_callers) good_ex_ids properties
                   db)

      | Entity_php.Class -> 
          let users = DbPHP.class_users_of_id id db in
          let extenders = DbPHP.class_extenders_of_id id db in

          let idfile = DbPHP.filename_of_id id db in
          let external_users = 
            exclude_ids_same_file (users ++ extenders) idfile db in

          let good_ex_ids = good_examples_of_use external_users db in
          let properties = [] in (* TODO *)

          Some (id, mk_entity 
                   ~root id 
                   (List.length external_users) good_ex_ids properties
                   db)



      | Entity_php.Interface -> 
          let users = DbPHP.class_implementers_of_id id db in

          let idfile = DbPHP.filename_of_id id db in
          let external_users = 
            exclude_ids_same_file (users) idfile db in

          let good_ex_ids = good_examples_of_use external_users db in
          let properties = [] in (* TODO *)

          Some (id, mk_entity 
                   ~root id 
                   (List.length external_users) good_ex_ids properties
                   db)

      (* TODO *)
      | Entity_php.ClassConstant
      | Entity_php.ClassVariable

      | Entity_php.XhpDecl
          

          
      | Entity_php.IdMisc

      | Entity_php.StmtList -> 
          None

    )
  in
  (* phase 2: adding the correct cross reference information *)
  let entities_arr = Array.of_list entities in

  let h_id_phpdb_to_id_db = Hashtbl.create 101 in
  entities_arr +> Array.iteri (fun id_db (id_phpdb, e) ->
    Hashtbl.add h_id_phpdb_to_id_db id_phpdb id_db;
  );
  let entities_arr =  entities_arr +> Array.map snd in
  let entities_arr =
    entities_arr +> Array.map (fun e ->
      let ids_phpdb = e.Db.e_good_examples_of_use in
      e.Db.e_good_examples_of_use <- 
        ids_phpdb +> List.map (fun id_phpdb -> 
          Hashtbl.find h_id_phpdb_to_id_db (Entity_php.Id id_phpdb)
        );
      e
    )
  in
  (* our current method/field analysis is imprecise; need to compensate back *)
  Db.adjust_method_or_field_external_users entities_arr;

  let dirs = dirs +> List.map (fun s -> 
    Common.filename_without_leading_path root s) in
  let dirs = Db.alldirs_and_parent_dirs_of_relative_dirs dirs in

  (* Note that Database_code requires readable paths, hence the
   * filename_without_leading_path below
   *)
  { Database_code.
    root = root;

    dirs = dirs +> List.map (fun d -> 
      d
      , 0); (* TODO *)
    files = files +> List.map (fun f -> 
      Common.filename_without_leading_path root f
      , 0); (* TODO *)
    
    entities = entities_arr;
  }
