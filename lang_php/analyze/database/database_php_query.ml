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

(* for fields *)
open Ast_php
open Database_php

module Flag = Flag_analyze_php
module Db = Database_php

module EC = Entity_php
module CG = Callgraph_php



(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common2.mk_pr2_wrappers Flag.verbose_database

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Query database *)
(*****************************************************************************)

(* note that Glimpse by default does a wholeword search but as '_'
 * is considered part word boundary, searching for instance for
 * jobs_get_mba_schools_  will return files
 *)
let glimpse_get_matching_files str db  =
  Glimpse.glimpse str (glimpse_metapath_of_database db)


(*****************************************************************************)
(* Callgraph wrappers *)
(*****************************************************************************)
let rec calltree_callers_of_f ~depth ~preferences id db = 
  let namefunc id = Db.name_of_id id db in
  let callersfunc id = Db.callers_of_id id db in
  let fullid_info id = db.fullid_of_id#assoc id in
  CG.calltree_callers_of_f id ~depth  ~parent_and_extra_opt:None 
    ~namefunc ~callersfunc ~fullid_info    ~preferences


let rec calltree_callees_of_f ~depth ~preferences id db = 
  let namefunc id = Db.name_of_id id db in
  let calleesfunc id = Db.callees_of_id id db in
  let fullid_info id = db.fullid_of_id#assoc id in
  CG.calltree_callees_of_f id ~depth ~parent_and_extra_opt:None
    ~namefunc ~calleesfunc ~fullid_info    ~preferences



(*****************************************************************************)
(* Actions *)
(*****************************************************************************)


let glimpse_query_files_db s dir = 
  Database_php.with_db ~metapath:dir (fun db -> 
    let res = glimpse_get_matching_files s db in
    pr2_gen res;
  )

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [

  "-test_query_glimpse_files", " <s> <dir>",
  Common.mk_action_2_arg glimpse_query_files_db;
]
