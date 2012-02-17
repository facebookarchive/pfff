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

module Ast  = Ast_php
module Flag = Flag_analyze_php
module V = Visitor_php
module N = Namespace_php
module Db = Database_php
module TH   = Token_helpers_php
module EC   = Entity_php
module CG   = Callgraph_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let _errors = ref []

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_database

let pr2_err s = 
  Common.pr2 s;
  Common.push2 s _errors;
  ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* should perhaps be moved in database_php.ml ? *)

(* Improve change of locality for analysis by sorting the files.
 * The default ordering of berkeley DB is based on filename but also
 * on their length, so not the best order. 
 * 
 * update: does it really help ?
 *)
let iter_files db f = 
  db.file_to_topids#tolist 
  +> Common.sortgen_by_key_lowfirst (* does it really help ? real opti ? *)
  +> Common_extra.progress ~show:!Flag.verbose_database (fun k ->
    List.iter (fun (file, ids) -> 
      k ();
      try f (file, ids)
      with exn -> 
        pr2 (Common.exn_to_s_with_backtrace exn);
        pr2_err (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
    ));
  ()

let iter_files_and_topids db f = 
  iter_files db (fun (file, ids) -> 
    ids +> List.iter (fun id -> f id file)
  )

let iter_files_and_ids db f = 
  iter_files_and_topids db (fun id file ->
    Db.recurse_children (fun id -> f id file) db id;
  )

(* todo: refactor the code of database_php_build.ml so needs
 * less this specific class case
 *)
let users_of_class_in_any any =
  Defs_uses_php.uses_of_any any +> Common.map_filter (fun (kind, name) ->
    match kind with
    | Database_code.Class _ -> Some name
    | _ -> None
  )

(*****************************************************************************)
(* Helpers ast and tokens *)
(*****************************************************************************)

(* 
 * Extract some position information from a toplevel element so can then
 * refer to this element through its 'fullid'. Of course we need to have
 * at least one valid info for this toplevel element.
 *)

exception NoII

(* TODO assume the ii are sorted by pos ? *)
let rec first_filepos_origin ii = 
  match ii with
  | [] -> raise NoII
  | x::xs -> 
      if Ast.is_origintok x 
      then x +> Ast.parse_info_of_info +> EC.filepos_of_parse_info
      else first_filepos_origin xs

let (fpos_of_toplevel: Ast.toplevel -> Entity_php.filepos) = fun top -> 
  let allii = Lib_parsing_php.ii_of_any (Toplevel top) in
  first_filepos_origin allii 

let (fpos_of_idast: Ast_php.entity -> Entity_php.filepos) = fun ast ->
  let allii = Lib_parsing_php.ii_of_any (Entity ast) in
  first_filepos_origin allii
  

(* less: similar func in lib_parsing ? *)
let prange_of_origin_tokens toks = 
  let toks = List.filter TH.is_origin toks in
  let ii = toks +> List.map TH.info_of_tok in
  
  try 
    let (min, max) = Lib_parsing_php.min_max_ii_by_pos ii in
    Ast.parse_info_of_info min, Ast.parse_info_of_info max
  with _ -> 
    (Parse_info.fake_parse_info, Parse_info.fake_parse_info)


let first_comment ast toks =
  let ii = Lib_parsing_php.ii_of_any (Toplevel ast) in
  let (min, max) = Lib_parsing_php.min_max_ii_by_pos ii in

  let min = Ast_php.parse_info_of_info min in

  let toks_before_min = 
    toks +> List.filter (fun tok ->
      Token_helpers_php.pos_of_tok tok < min.Parse_info.charpos
    ) +> List.rev
  in

  (* note that the tokens in toks_before_min are in reverse order.
   * Note also that for one-liner comments the tokens contain
   * a newline. so //comment\nfunctionfoo will return 2 tokens,
   * [T_COMMENT("//comment\n"); T_FUNCTION(...)].
   *)
  let comment_info = 
    match toks_before_min with
    | Parser_php.TNewline i1::
        (Parser_php.T_COMMENT i2|Parser_php.T_DOC_COMMENT i2)::xs ->
        if Ast_php.col_of_info i2 = 0
        then Some (Ast_php.parse_info_of_info i2)
        else None

    (* for one-liner comment, there is no newline token before *)
    | Parser_php.T_COMMENT i2::xs ->
        if Ast_php.col_of_info i2 = 0
        then Some (Ast_php.parse_info_of_info i2)
        else None
    | _ -> None
  in
  comment_info

(*****************************************************************************)
(* Helpers to add stuff in database *)
(*****************************************************************************)

(* The goal of those functions are to provide wrappers around adding stuff
 * in the db that automatically handle the consistency of the db when for
 * instance we have both a table and its inverted index.
 *)

let get_newid db =
  let id = EC.Id db.next_free_id in
  db.next_free_id <- db.next_free_id + 1;
  id

let add_toplevel2 file (top, info_item) db = 
  let (str,toks) = info_item in

  let newfullid = 
    try 
      fpos_of_toplevel top 
    with NoII -> 
      pr2_err ("PB: pb NoII with:" ^ file);
      (*
      toput before: let cnt = ref 0 

      pr2 str;
      pr2 ("using info from token then:");
      (try 
          first_filepos_origin_token toks
      with NoII -> 
        pr2 ("PB: pb NoII with tokens too:");
        pr2 (
          toks +> List.map Token_helpers.str_of_tok
               +> Common.join " "
        );
          
        (* if a macro expands to multiple declaration, then have a serie
         * of elements in the ast without connection to the original file.
         * but must still try to generate some fake id for them ?
         *)
        incr cnt;
        { Entity_c.file = file;
          line = -(!cnt);
          column = 0;
        }
      )
      *)
      raise Todo
  in

  if db.id_of_fullid#haskey newfullid
  then begin
    (*
    let newid = db.id_of_fullid#assoc newfullid in
     let str2 = db.str_of_ast#assoc newid in
    pr2_gen (newid, (str, str2));
    *)
    failwith "fullid already in database"
  end;

  let newid = get_newid db in

  let range = prange_of_origin_tokens toks in

  db.fullid_of_id#add2 (newid, newfullid);
  db.id_of_fullid#add2 (newfullid, newid);

  db.defs.toplevels#add2 (newid, top);

  db.defs.str_of_topid#add2 (newid, str);
  db.defs.tokens_of_topid#add2 (newid, toks);
  db.defs.range_of_topid#add2 (newid, range);

  db.defs.extra#add2 (newid, Database_php.default_extra_id_info);

  newid


let (add_filename_and_topids: (filename * id list) -> database -> unit) = 
 fun (file, ids) db -> 
   db.file_to_topids#add2 (file, ids);
   ()


(* have to update tables similar to add_toplevel *)
let (add_nested_id_and_ast: enclosing_id:id -> Ast_php.entity ->database -> id)
 = fun ~enclosing_id ast db ->

  (* fullid_of_id, extra,   and the specific asts, enclosing and children *)
  let newfullid = 
    try 
      fpos_of_idast ast
    with NoII -> 
      failwith ("PB: pb NoII");
  in
  if db.id_of_fullid#haskey newfullid
  then failwith "fullid already in database";

  let newid = get_newid db in

  db.fullid_of_id#add2 (newid, newfullid);
  db.id_of_fullid#add2 (newfullid, newid);
  db.enclosing_id#add2 (newid, enclosing_id);
  
  db.children_ids#apply_with_default2 enclosing_id
    (fun old -> newid::old) (fun() -> []);

  db.defs.asts#add2 (newid, ast);
  db.defs.extra#add2 (newid, Database_php.default_extra_id_info);
  newid

(*---------------------------------------------------------------------------*)
let (add_def: 
 (id_string * id_kind * id * Ast_php.name option) -> database -> unit) =  
  fun (idstr, idkind, id, nameopt) db -> 

    db.defs.name_defs#apply_with_default2 idstr 
      (fun old -> id::old) (fun() -> []);

    (* the same id can not have multiple kind in PHP, as it can not contain
     * for instance both a variable and struct declaration as in ugly C.
     *)
    if db.defs.id_kind#haskey id 
    then failwith ("WEIRD: An id cant have multiple kinds:" ^ 
                      Db.str_of_id id db);
    db.defs.id_kind#add2 (id, idkind);

    (* old: add2 (id, idkind); *)
    db.defs.id_name#add2 (id, idstr);

    nameopt +> Common.do_option (fun name ->
      db.defs.id_phpname#add2 (id, name);
    );
    db.symbols#add2 (idstr, ());
    ()

(*---------------------------------------------------------------------------*)
(* add_callees_of_f, add in callee table also add in its reversed index
 * caller table. Should be called only one time for each idfpos.  
 * 
 * history: was taking string list, then string set, and now string wrap list.
 * Was quite fast at the beginning, but when adding more and more information
 * (such as remembering all the instances), it gets slower so needed 
 * some optimisations.
 * 
 * optimisations:
 *   - factorization: 
 *     have optimized value (DirectCallIsOpt) that factorize things so
 *     need less call to the callers#add2 method.
 * 
 *   - caching:  
 *     augment buffer size cache for the callers_of_f table as it is very
 *     randomly accessed but often some entries are reaccessed 
 *     and augmented and so want to avoid the disk IO and marshalling
 *     (cf Database.open_db).
 * 
 *   - locality:
 *     try augment the chance of locality by processing files in a certain 
 *     order so that the chance that have similar caller/callee in each file
 *     are quite important. Then the cache will work even better. 
 * 
 *   - compression:
 *     have optmized value that reduce the size of individual 
 *     information, for instance using some id_opt instead of id that
 *     contains lengthy filename, or avoid redundancy that could 
 *     be recomputed easily (from the ast, or from reverse table)
 *     or for instance using position_call_site
 *     instead of full expression or Ast_c.info that are big. 
 *     I guess this is quite useful especially for the caller table 
 *     as even with the augmented buffer size cache, we still need 
 *     sometimes to read entries on the disk and if those entries are big
 *     the problem. 
 *     
 *     So opti: caller/callee again, with 
 *     SameFile of int(*charpos*) | GeneralPos of info ?
 *     Also can just store the offset, no need the (lenthy) filename.
 * 
 *   - todo? preloading of the kinds and name tables used in 
 *     ids_with_kinds__of_string (but apparently not where is bottleneck
 *     on the long run). Put in create_db signature too. How ? simply do:
 *      let oldx = 
 *      let oldy = 
 *      let db = { db with ... }
 *      :)
 *     Could also check that size on disk not too big ? or check the number
 *     of keys ? 
 * 
 *)
let (add_callees_of_id2: (id * (N.nameS Ast_php.wrap list)) -> database -> unit)=
 fun (idcaller, funcalls) db -> 

   (* old: assert(Common.is_set funcs); still true but make less sense
    * now that pass all instances 
    * 
    * old: let directfuncs = funcs +> List.map (fun s -> CG.DirectCall s) in
    * update: now resolve here the possible callees using global information.
    * WWW also give score when multiple possible defs for same string.
    *)
(* old:
   let directfuncs = 
     funcs +> List.map (fun (s, wrap) -> 
       let candidates = 
         Database_c_query.get_functions_or_macros_ids__of_string s db 
       in
       (s,wrap), candidates
     )
   in
   let directcalls = 
     directfuncs +> List.map (fun (swrap, ids) -> 
       ids +> List.map (fun id -> CG.DirectCallTo (id, swrap))
     ) +> List.flatten
   in
*)


   let grouped_instances_same_ident_with_candidates = 
     Common.profile_code "DB.add_callees_of_f_p1" (fun () -> 

       let grouped_instances_same_name = 
         Common.group_by_mapped_key (fun (name, ii) -> name) funcalls in

       grouped_instances_same_name +> List.map (fun (name, name_ii_list) -> 
         let candidates = 
           match name with
           | N.NameS s -> 
               (* can do a few IOs on bdb tables #defs and #kinds *)
               Db.function_ids__of_string s db

           (* Note that because A::foo() and B::foo() may refer to the same
            * id (when B inherits from A without redefining foo), 
            * and because above we group by name, we may generate
            * here for a caller multiple direct call references to the same 
            * id via DirectCallToOpt below. Some invariants can be 
            * broken because of that. So take care for instance in 
            * Callgraph_php.callersinfo_opt_to_callersinfo to handle such
            * corner cases. Another solution would be here to better 
            * group and to identify that the different names 
            * A::foo() and B::foo() actually refers to the same
            * id.
            *)
           | N.NameQualifiedS (sclass, smethod) ->
               (* TODO *)
               []
               (*
               Db.static_function_ids_of_strings ~theclass:sclass smethod db
               *)
         in
         let s = N.nameS name in
         if null candidates && !Flag.show_analyze_error
         then pr2 (spf "PB: no candidate for function call: %s" s);

         name, name_ii_list |> List.map snd, candidates
       ) 
     )
   in

   (* updating the callees_of_f table *)

   let directcalls =   
     grouped_instances_same_ident_with_candidates 
     |> List.map (fun (a,b,c) -> CG.DirectCallToOpt (a,b,c))
   in
   
   Common.profile_code "DB.add_callees_of_f_p2" (fun () -> 
     db.uses.callees_of_f#add2 (idcaller, directcalls);
   );

   (* updating the callers_of_f table *)

   (* Do we need to use cons or insert_set below ? What is the interest 
    * of insert_set ? to avoid duplication but normally should not call
    * two times add_callees_of_f and so should never add again the same
    * id as the caller of another id.
    * In doubt, if don't trust that the caller adequatly call 
    * add_callees_of_f, then call Database_c.check_db to check
    * the db invariants.
    * 
    * note: This is what takes the most time. This is because 
    * we have to get the value from many different id which are
    * spread into the database hence I guess some disk seek that slow
    * down the process. The numbers of calls to callers_of_f#add
    * is approximately the same than the calls to callees_of_f but
    * in one case we seek far more.
    * 
    *)
(* old:
   directfuncs +> List.iter (fun (swrap, ids) -> 
     ids +> List.iter (fun callee -> 
       db.callers_of_f#apply_with_default callee
         (fun old -> 
           (* insert_set ? *)
           Common.cons (CG.DirectCallerIs (idcaller, swrap)) old
         ) (fun() -> []) +> ignore;
     );
   );
*)

   Common.profile_code "DB.add_callees_of_f_p3" (fun () -> 
   grouped_instances_same_ident_with_candidates 
   +> List.iter (fun (name, infos, ids) -> 
     let nbinstances = List.length infos in 
     (* iter on possible callee candidates *)
     ids +> List.iter (fun callee -> 
       db.uses.callers_of_f#apply_with_default callee
         (fun old -> 
           (* old: idcaller (name, infos), but can be retrieved from
            * reversed table. 
            *)
           Common.cons (CG.DirectCallerIsOpt (idcaller, name, nbinstances)) old
         ) (fun() -> []) +> ignore;
     ));
   );
   ()

let add_callees_of_id a b = 
  Common.profile_code "Db.add_callees_of_f" (fun () -> 
    add_callees_of_id2 a b
  )

(*---------------------------------------------------------------------------*)
(* Similar function but for the indirect function calling part.
 * Also add in reverse index. 
 * 
 * Note that unlike the previous function add_callees_of_f2, 
 * we call add_callees_of_f_indirect multiple times
 * for the same idcaller. Each time for a different indirect function pointer
 * call site which have possibly itself a different set of candidates. 
 * This is why even if we limit in index_db4 the number of candidates
 * to let's say 100, we can still have as in Linux for getfpreg()
 * 6400 callees as we can have 64 indirect pointer function call sites
 * each with 100 candidates.
 * 
 * todo? So need also a way to limit the size of the entries so that
 * a few outliers does not cause the full index_db4 process to slow down.
 * Moreover we can also use the partial_caller info.
 * 
 *)
let add_methodcallees_of_id (idcaller, methods) db = 
  raise Todo
