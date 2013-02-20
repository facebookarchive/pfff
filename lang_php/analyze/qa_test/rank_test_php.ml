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

module Ast = Ast_php
module V = Visitor_php

module Db = Database_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * A good metric for software quality is the percentage of code "covered"
 * by testing. If the code is not compltely covered, then we must decide
 * which code we should test first. The goal of this module is to compute 
 * the set of functions that are considered important, according to 
 * the CodeRank algorithm, and not covered, according to dynamic analysis 
 * information from xdebug.
 * 
 * src: eletuchy idea.
 * 
 * As opposed to Test_smells_php, here we need to do global analysis
 * (CodeRank needs that), so we will work on the pfff_db and so we 
 * require the user to first have build a pfff database.
 * 
 * We also cross-check with results from static analysis, that is
 * which functions are directly (statically) mentionned in unit test files
 * but reported here as not covered.
 * 
 * See also the treemap visualisation of test coverage in 
 * pfff/facebook/visual/visual_treemap_php.ml (for the moment using 
 * only static coverage, which is probably more appropriate for 
 * unit testing coverage)
 *)


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* assumes have a database with the coverage information (see
 * the -index_xdebug_db command line flag of pfff_db)
 *)
let (functions_not_covered_and_high_code_rank: 
      ?topn:int -> 
      is_test_file:(Common.filename -> bool) -> 
      Db.database -> unit) =
 fun ?(topn=30) ~is_test_file db ->
   let ranks = Code_rank_php.build_code_ranks db in

   let xs = ranks.Code_rank_php.function_ranks#tolist in
   let sorted = Common.sort_by_val_highfirst xs in

   let is_dynamically_covered = 
     Coverage_dynamic_php.mk_is_covered_by_test db in
   let is_statically_covered = 
     Coverage_static_php.mk_is_covered_by_test 
       ~is_test_file db in
   
   let is_not_covered = sorted +> List.filter (fun (id, rank) ->
     not (is_dynamically_covered id)
   )
   in
   (* sanity check *)
   is_not_covered +> List.iter (fun (id, rank) ->
     if is_statically_covered id 
     then 
       let name = Db.name_of_id id db in
       pr2 (spf "WEIRD: function %s is considered both statically covered by test"
               name);
       pr2 (" and dynamically not covered");
       (* failwith (spf "PB with: %s" (Db.str_of_id id db)) *)
   );

   pr "Functions not covered and with high rank";
   is_not_covered +> Common.take_safe topn +> Common.index_list_1
   +> List.iter (fun ((id, testrank), nrank)  ->
     let name = Db.name_of_id id db in
     let file = Db.filename_of_id id db in
     let line = Db.line_of_id id db in
     
     pr (spf "%02d %s, at %s:%d, with rank %f" nrank name file line testrank);
   )

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  "-test_rank", " <db> <topn>",
  Common.mk_action_2_arg (fun metapath topn ->
    Db.with_db ~metapath (fun db ->
      let topn = s_to_i topn in
      functions_not_covered_and_high_code_rank 
        ~topn 
        ~is_test_file:(fun file -> raise Todo )
        db;
      ()
    ));
]
