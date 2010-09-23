(*s: coverage_dynamic_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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

open Ast_php

module Ast = Ast_php
module V = Visitor_php

module Db = Database_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Abstract away a little the way we determine whether a function is 
 * covered by a unit test.
 * 
 * Right now an id is 'covered' if it has its type set in the db.
 * This is done by -index_db_xdebug.
 * 
 * todo: could have a dynamicaly_covered field in Database_php, that
 * cache this information.
 * 
 *)

let (mk_is_covered_by_test: Database_php.database -> (Entity_php.id -> bool)) =
 fun db ->
  (fun id ->

    try 
      let _ = db.Db.defs.Db.id_type#assoc id  in
      true
   with Not_found -> 
     (* I also consider functions with multiple definitions (such as 
      * apc_fetch) as covered, as the -index_xdebug_db analysis
      * does not address for now such cases.
      * todo: when -index_xdebug_db is improved, should update this code.
      *)
       let funcname = Db.name_of_id id db in
       let ids_with_name = Db.function_ids__of_string funcname db in
       List.length ids_with_name > 1
  )
(*e: coverage_dynamic_php.ml *)
