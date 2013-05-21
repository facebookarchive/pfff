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
open Common2
open Common

open Ast_php
open Database_php

module EC = Entity_php
module Db = Database_php
module DbQ = Database_php_query
module V = Visitor_php
module Ast = Ast_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Allow to see if progress when improve some analysis.
 * Can improve statistics by improving parsing, typing, or analysis.
 * 
 * less: Give start per db, dir, or file ? but for file I can use GUI
 * and color to see if pbs, so useful only for dir.
 *) 

(*---------------------------------------------------------------------------*)
(* Stat on parsing, based on ast and NotParsedCorrectly. Use range info.
 *  
 * On www with original parsing_stat I get:
 * ????
 * 
 * With the stat working on the database we get:
 *   ???
 * With different algorithm for diff:
 *   ???
 * 
 * Why diff ? TODO
 * How check that have not same result than what -parse_all was saying ?
 * 
 * less: can also maybe print in one file all this badly parsed code
 * 
 * Cf tests_score.org.
 * time: ???
 * 
 * access two fields of db, range and objects, so can take quite some time
 * on big database such as linux.
 *)

let parsing_stat_db db = 

  let good, bad = ref 0, ref 0 in
  let numperfect = ref 0 in
  let numtoplevel = ref 0 in
  let numnotparsedcorrectly = ref 0 in

  db.file_to_topids#tolist +> Common2.index_list_and_total +>
  (fun xs -> Common2.execute_and_show_progress (List.length xs) (fun k -> 
   xs 
   +> List.iter 
    (fun ((file, ids), i, total) -> 

      (* pr2 (spf "ANALYZING: %s %d/%d " file i total); *)
      k();
      let is_perfect = ref true in

      ids +> List.iter (fun id -> 

        let (p1, p2) = db.defs.range_of_topid#assoc id in

        let l1 = p1.Parse_info.line in
        let l2 = p2.Parse_info.line in

        incr numtoplevel;

        if(p1 <> Parse_info.fake_parse_info && 
           p2 <> Parse_info.fake_parse_info) 
        then begin
          assert (l1 <= l2);
          assert(p1.Parse_info.file = p2.Parse_info.file);

          let diff = 
            if l2 = l1 
            then 1
            else (l2 - l1)
          in

          let ast = db.defs.toplevels#assoc id in

          (match ast with
          | NotParsedCorrectly _ -> 
              bad += diff;
              is_perfect := false;
              incr numnotparsedcorrectly;
          | _ -> 
              good += diff
          );
        end
        else 
          pr2_once "one of element is a fake parse info"
        ;
      );
      if !is_perfect then incr numperfect;
      
    )));
  let percent = Common2.pourcent_good_bad_float !good !bad in

  pr2 ("parsing stat");
  pr2 (spf "perfect = %d" !numperfect);
  pr2 (spf "nb good = %d, nb bad = %d, %02.2f%%"  !good !bad percent);
  pr2 (spf "number of items = %d" !numtoplevel);
  pr2 (spf "number of items not correctly parsed = %d" !numnotparsedcorrectly);
  ()

(*---------------------------------------------------------------------------*)
(* time on www:  ??
 *)
let callgraph_stat_db db = 
  raise Todo

(*

(* old: 
 *  db.callers_of_f#tolist +>
 * (fun xs -> Common.execute_and_show_progress (List.length xs) (fun k -> 
 *  xs +> List.iter 
 *  (fun (key, xs) -> 
 * => but tolist use too much memory on this table
 * 
 * old: 
 *   let len =  assoc#length in
 * => but also too slow cos currently length also need to go through
 * all elements (well all keys but as callers/callees have huge number
 * of keys, it is still very slow and for that we don't see any
 * progress so just better put fake lenght, execute_and_show_progress
 * does not care anyway.
 *)

  (* -------------------------------------- *)
  (* callee *)
  (* -------------------------------------- *)
  let direct, indirect = ref 0, ref 0 in
  let distribution = Hashtbl.create 101 in

  let max_with_elem = ref 0, ref "" in

  let assoc = db.callees_of_f in
  let len = 10 in (* old: assoc#length *)
  Common.execute_and_show_progress len (fun k -> 
    assoc#iter (fun (key, xs) -> 
     k();
     let key_str = Database_c.name_of_id key db in

     let num_indirect = ref 0 in

     xs +> List.iter (fun v -> 
       match v with
       | Callgraph_c.DirectCallToOpt _ -> incr direct;
       | Callgraph_c.IndirectFuncPtCallToOpt _ -> incr indirect;
           incr num_indirect;
     );
     Common.hupdate_default !num_indirect 
       (fun old -> old + 1) (fun() -> 0) distribution;
     Common.update_max_with_elem max_with_elem
       ~is_better:(fun newi old -> newi > !old) (!num_indirect, key_str);

   ));
  let percent = 
    Common.pourcent_good_bad_float !direct !indirect in
  pr2 ("callee stat");
  pr2 (spf "nb direct = %d, nb indirect = %d, %02.2f%%"
          !direct !indirect percent);
  distribution +> Common.hash_to_list +> List.iter (fun (k,v) -> 
    pr (spf "%d -> %d" k v);
  );
  pr2 (spf "max: %d %s" (!(fst max_with_elem)) (!(snd max_with_elem)));
  Common.with_open_outfile (metapath_of_database db ^ "/stat_callee.txt")
  (fun (pr,_chan) -> 
    distribution +> Common.hash_to_list +> List.iter (fun (k,v) -> 
      pr (spf "%d %d\n" k v);
    );
    pr (spf "#max: %d %s" (!(fst max_with_elem)) (!(snd max_with_elem)));
  );



  (* -------------------------------------- *)
  (* caller *)
  (* -------------------------------------- *)
  let direct, indirect = ref 0, ref 0 in
  let distribution = Hashtbl.create 101 in

  let max_with_elem = ref 0, ref "" in

  let assoc = db.callers_of_f in
  let len = 10 in 
  Common.execute_and_show_progress len (fun k -> 
    assoc#iter (fun (key, xs) -> 
     k();

     let key_str = Database_c.name_of_id key db in

     let num_indirect = ref 0 in
     xs +> List.iter (fun v -> 
       match v with
       | Callgraph_c.DirectCallerIsOpt _ -> incr direct;
       | Callgraph_c.IndirectFuncPtCallerIsOpt _ -> incr indirect;
           incr num_indirect;
     );
     Common.hupdate_default !num_indirect 
       (fun old -> old + 1) (fun() -> 0) distribution;
     Common.update_max_with_elem max_with_elem
       ~is_better:(fun newi old -> newi > !old) (!num_indirect, key_str);

   ));
  let percent = 
    Common.pourcent_good_bad_float !direct !indirect in
  pr2 ("caller stat");

  pr2 (spf "nb direct = %d, nb indirect = %d, %02.2f%%"
          !direct !indirect percent);
  distribution +> Common.hash_to_list +> List.iter (fun (k,v) -> 
    pr2 (spf "%d -> %d" k v);
  );
  pr2 (spf "max: %d %s" (!(fst max_with_elem)) (!(snd max_with_elem)));

  Common.with_open_outfile (metapath_of_database db ^ "/stat_caller.txt")
  (fun (pr,_chan) -> 
    distribution +> Common.hash_to_list +> List.iter (fun (k,v) -> 
      pr (spf "%d %d\n" k v);
    );
    pr (spf "#max: %d %s" (!(fst max_with_elem)) (!(snd max_with_elem)));
  );



  ()

*)

(*---------------------------------------------------------------------------*)
let extra_stat_db db = 

(*
  let total = ref 0 in
  let partial_callers = ref 0 in
  let partial_callees = ref 0 in

  let assoc = db.extra in
  let len = 10 in (* old: assoc#length *)
  Common.execute_and_show_progress len (fun k -> 
   assoc#iter (fun (key, extra) -> 
     k();
     incr total;
     if extra.partial_callees then incr partial_callees;
     if extra.partial_callers then incr partial_callers;
   ));

  let percent1= 
    Common.pourcent_float !partial_callers !total in
  let percent2= 
    Common.pourcent_float !partial_callees !total in
  pr2 ("extra stat");

  pr2 (spf "nb defs = %d" (db.defs#length));
  pr2 (spf "nb ftype = %d" (db.ftype#length));

  pr2 (spf "nb partial callers = %d, total = %d, %02.2f%%"
          !partial_callers !total percent1);
  pr2 (spf "nb partial callees = %d, total = %d, %02.2f%%"
          !partial_callees !total percent2);
*)
  
  ()

(*---------------------------------------------------------------------------*)
let fields_stat_db db =

  let hcount = Hashtbl.create 101 in

  db.file_to_topids#tolist +> (fun xs -> 
  Common2.execute_and_show_progress (List.length xs) (fun k -> 
    xs +> List.iter (fun (file, ids)-> 
     k();
     ids +> List.iter (fun id ->

      let ast = db.Db.defs.Db.toplevels#assoc id in

      let hooks = { V.default_visitor with
        V.kexpr = (fun (k,vx) v ->
          match v with
          | ArrayGet (var2, expr_bracket) ->
              (match var2, Ast.unbracket expr_bracket with
              | (IdVar (dname, scope), 
                Some (Sc (C (Ast.String (s, info))))) ->
                  Common2.hupdate_default s Common2.add1 Common2.cst_zero hcount;
              | _ -> k v
              )
          | _ -> k v
        );
      }
      in
      (V.mk_visitor hooks) (Toplevel ast);
     )
    )));
  let xs = hcount +> Common.hash_to_list +> Common.sort_by_val_highfirst in
  xs +> List.iter (fun (k,v) ->
    pr (spf "%20s -> %d" k v)
  );
  ()

(*---------------------------------------------------------------------------*)
let includes_stat_db db = 
  db.uses.includers_of_file#iter (fun (file, includers) -> 
    pr2 (spf "%s is included by:" file );
    includers |> List.iter (fun file -> pr2 (" " ^ file));
  );
  db.uses.includees_of_file#iter (fun (file, includees) -> 
    pr2 (spf "%s includes:" file );
    includees |> List.iter (fun file -> pr2 (" " ^ file));
  );
  ()

(*---------------------------------------------------------------------------*)
let all_stat_db db = 
  parsing_stat_db db;
(*
  typing_stat_db db;
  callgraph_stat_db db;
  extra_stat_db db;
*)
  ()



(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [

  "-parsing_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname parsing_stat_db);
  "-callgraph_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname callgraph_stat_db);

  "-fields_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname fields_stat_db);

  "-includes_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname includes_stat_db);

  "-extra_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname extra_stat_db);

  "-all_stat_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname all_stat_db);
]
