(*s: deadcode_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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

module Flag = Flag_analyze_php
module Ast = Ast_php
module PI = Parse_info

module V = Visitor_php
module T = Type_php
module N = Namespace_php

module Db = Database_php
(* module DbQ = Database_php_query *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * For the moment we just look if the function has no callers and is not
 * mentionned in some strings somewhere (but look for exact string).
 * 
 * update: We do a few heuristics for function pointer that
 * are passed via the hooks is_probable_dynamic_funcname.
 * See aliasing_function_php.ml which was used to compute 
 * some of the value for this hook.
 * 
 * After the heuristic suggested by lklots, got now 
 * 2421 dead functions. I think it was 3500 at the beginning when
 * I was not even doing the exact-string-match false positive heuristic.
 * 
 * 
 * history:
 * - simple reaper
 * - better heuristic for funcvar
 * - better blame -C, and filter third-party stuff
 * - do not generate patch for files where 
 *   would return multiple match on certain deadcode functions
 * - add date information to the patch filename
 *   so can later decide to apply first old dead code patches
 * - do fixpoint. Also do CEs when using nested ast_ids
 * - can now use a whitelist generated from phproflive
 * - dead classes
 * 
 * Some of the code specific to facebook is in facebook/ and in main_db.ml
 * 
 * Here is the first commit :) 
 * 
 *   commit e52575f16c70a4507125a1e3a662456cb36d057d
 *   Author: dcorson <dcorson@2c7ba8d8-a2f7-0310-a573-de162e16dcc7>
 *   Date:   2 weeks ago
 *   
 *   initial batch of dead code from ~pad's static analyzer
 *   
 *   Summary: i also further deleted a couple files that were now empty
 *   like lib/deprecated.php (removing any includes of them as well).
 *   this is a test run of 30 files.  ~pad has over a thousand more that
 *   he will probably be sending out to ppl that they blame to, after we
 *   see how this initial batch goes.
 *   
 *   Reviewed By: epriestley
 *   
 *   Test Plan: grep for bunch of dead functions and all dead files,
 *   browse site and use i18n stuff and import a blog.
 *   
 *   Revert Plan: ok
 *   
 *   git-svn-id: svn+ssh://tubbs/svnroot/tfb/trunk/www@202716 2c7ba8d8-a2f7-0310-a573-de162e16dcc7
 * 
 * 
 * The deadcode analysis can cope with code using XHP as XHP maintains
 * (mostly) line accuracy and that we generate program transformations 
 * at a coarse-grained level, using line information.
 * 
 * Note that you need the annotation indexing while building the code database
 * as some code can have some @called-from-phpsh comments.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let verbose_reaper = ref false
let pr2, pr2_once = Common2.mk_pr2_wrappers verbose_reaper

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

type hooks = {
  (* to remove certain false positives *)
  is_probable_dynamic_funcname: string -> bool;
  is_probable_dynamic_classname: string -> bool;

  (* to avoid generating patches for code which does not have a valid 
   * git owner anymore (the guy left the company for instance ...)
   *)
  is_valid_author: string -> bool;
  (* to avoid generating patches for certain files, such as code in 
   * third party libraries or auto generated code. Will be called
   * with a filename without leading project.
   *)
  is_valid_file: filename -> bool;

  skip_revs:Lib_vcs.versionid list;

  (* code annotated with @not-dead-code should not be considered *)
  false_positive_deadcode_annotations: Annotation_php.annotation list;

  (* config *)
  print_diff: bool;
  with_blame: bool;
  cache_git_blame: bool;
  (* place where we would put the generated patches *)
  patches_path: Common.dirname;

 skip_patch_generation_for_file: Common.filename -> bool;
}

let default_hooks = {
  is_probable_dynamic_funcname = (fun s -> false);
  is_probable_dynamic_classname = (fun s -> false);

  is_valid_author  = (fun s -> true);
  is_valid_file = (fun filename -> true);
  skip_revs = [];
  false_positive_deadcode_annotations = [
    Annotation_php.CalledFromPhpsh;
    Annotation_php.CalledOutsideTfb;
    Annotation_php.CalledDynamically;
    Annotation_php.NotDeadCode;
    Annotation_php.CalledDynamically;
  ];
  print_diff = true;
  with_blame = false;
  cache_git_blame = true;
  patches_path = "/tmp/deadcode_patches";

  skip_patch_generation_for_file = (fun file -> false);
}

type removed_lines = {
  just_code_lines: int list;
  code_and_comment_lines: int list;
}

type deadcode_patch_info = {
  file     : Common.filename; (* path relative to the project *)
  reviewer : string option; (* maybe nobody ... *)
  cc       : string option;
  date     : Common2.date_dmy;
}

type dead_ids_by_file = 
  (Common.filename * (string * Database_php.fullid * Database_php.id) list) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let group_ids_by_file ids db = 
  ids 
  +> List.map (fun id -> Db.name_of_id id db, db.Db.fullid_of_id#assoc id, id)
  +> Common.group_by_mapped_key (fun (s,fullid,id) -> fullid.Entity_php.file)

let ungroup_ids_by_file grouped_ids = 
  grouped_ids 
  +> List.map (fun (file, ids) -> 
       ids +> List.map (fun (s, fullid, id) -> id))
  +> List.flatten

(*****************************************************************************)
(* Deadcode analysis Helpers *)
(*****************************************************************************)

(* TODO: should also analyze dead methods, dead classes *)

let false_positive fid hooks db = 
  let extra = db.Db.defs.Db.extra#assoc fid in
  let s = Db.name_of_id fid db in

  match () with

  | _ when db.Db.strings#haskey s ->
      pr2 ("Probable indirect call (or because not yet parsed): " ^ s);
      true

  | _ when hooks.is_probable_dynamic_funcname s ->
      pr2 ("Probable dynamic call: " ^ s);
      true
  | _ when hooks.is_probable_dynamic_classname s ->
      pr2 ("Probable dynamic class: " ^ s);
      true

  | _ when hooks.false_positive_deadcode_annotations +> 
        List.exists (fun annot -> List.mem annot extra.Db.tags) ->

      pr2("tagged as @called-from-phpsh or related: " ^ s);
      true
            
  | _ ->
      false


let finding_dead_functions hooks db = 
  let dead_ids = ref [] in

  (* todo: Db.functions_or_static_methods_in_db *)
  Db.functions_in_db db +> List.iter (fun (idstr, ids) ->
    let s = idstr in

    (* todo? A::foo() and foo() will be currently agglomerated and 
     * so we may report false ambiguity. This is unfortunate, but 
     * because below we then use ids, and not string anymore, 
     * the analysis should run fine.
     *)
    if List.length ids > 1 
    then pr2 ("Ambiguous name: " ^ s);

    ids +> List.iter (fun id ->

      let file_project = Db.readable_filename_of_id id db in

      if hooks.is_valid_file file_project then begin

        let is_dead_func = 
            (* TODO what if recursive ? or mutually recursive but dead ? *)
          let callers = Db.callers_of_id id db in
          null callers && not (false_positive id hooks db)
        in
        if is_dead_func
        then begin
          pr2 (spf "DEAD FUNCTION: no caller for %s (in %s)" s file_project);
          Common.push2 (s, id) dead_ids;
        end
      end
    );
  );
  pr2 (spf "number of dead functions: %d" (List.length !dead_ids));
  !dead_ids

(* todo: factorize code with finding_dead_functions ? have notion
 * of entity and user of entities
 * 
 * todo: finding_dead_ constants/interface/globals/variables ?
 *)
let finding_dead_classes hooks db = 
  let dead_ids = ref [] in

  (* todo: Db.functions_or_static_methods_in_db *)
  Db.classes_in_db db +> List.iter (fun (idstr, ids) ->
    let s = idstr in

    if List.length ids > 1 
    then pr2 ("Ambiguous name: " ^ s);

    ids +> List.iter (fun id ->
      let file_project = Db.readable_filename_of_id id db in
      if hooks.is_valid_file file_project then begin
        let is_dead = 
          let users = Db.class_users_of_id id db in
          null users && not (false_positive id hooks db)
        in
        if is_dead
        then begin
          pr2 (spf "DEAD CLASS: no user/extender for %s (in %s)" s file_project);
          Common.push2 (s, id) dead_ids;
        end
      end
    );
  );
  pr2 (spf "number of dead classes: %d" (List.length !dead_ids));
  !dead_ids



(* 
 * Fixpoint per file. Just remember the current dead ids, then reiterate 
 * on the code but file by file, and get all ids in this file which
 * are functions and with all callers that are in the current set of
 * dead, and add in the set of dead, and fixpoint.
 *)
let deadcode_fixpoint_per_file all_dead_ids hooks db = 

  let grouped_by_file = group_ids_by_file all_dead_ids db in

  (* old: 
   *
   *  let hdead = Common.hashset_of_list all_dead_ids in
   * 
   * Previously I was considering all existing dead ids as the 
   * first set of dead ideas in the fixpoint. One problem is that
   * if patch A contains 1 regular dead function (no caller at all),
   * and patch B contains 1 regular dead function, then when we revisit 
   * A, maybe a function becomes dead just because its only callers is 
   * in B, and if we add this function in the deadcode patch for A, then
   * we introduce a dependency between patches which is not good for 
   * the user. So better to consider the dead functions in A 
   * as the starting point for the fixpoint analysis on A.
   *)
  
  grouped_by_file +> List.map (fun (file, funcs_and_ids) -> 
    (* Right now I do the fixpoint inside the file. Could also do
     * it more globally, because maybe the removal of one function in 
     * B should force to revisit what we have decided for A. 
     * But this would be far more expensive and also would introduce
     * too complex dependency between patches for the user to approve
     * (see comment above).
     *)

    let hdead = funcs_and_ids 
      +> List.map (fun (s, fullid, id) -> id) 
      +> Common.hashset_of_list 
    in

    let ids_file = 
      db.Db.file_to_topids#assoc file in

    let candidates = 
      ids_file +> List.filter (fun id -> 
        not (Hashtbl.mem hdead id)       && 
        Db.is_function_id id db          && 
        not (false_positive id hooks db) &&
        true
      )
    in
    
    let rec fix also_dead remaining = 
      let new_dead, remaining' = 
        remaining +> List.partition (fun id ->
          let callers = Db.callers_of_id id db in
          let idcallers = callers +> List.map Callgraph_php.id_of_callerinfo in
          let ids_not_dead = 
            idcallers +> Common.exclude (fun id2 -> 
              (* bugfix: put a && instead of || ... *)
              Hashtbl.mem hdead id2 || id = id2 (* recursive calls *)
            )
          in
          null ids_not_dead
        )
      in
      if null new_dead
      then also_dead (* fixpoint reached *)
      else begin
        new_dead +> List.iter (fun id -> Hashtbl.add hdead id true);
        fix (new_dead ++ also_dead) remaining'
      end
    in
    let new_dead = fix [] candidates in

    let info_new_dead = new_dead +> List.map (fun id ->
        let s = Db.name_of_id id db in
        let fullid = db.Db.fullid_of_id#assoc id in
        pr2 ("DEAD FUNCTION: no caller when do fixpoint for " ^ s);
        s, fullid, id
    )
    in
    (file, info_new_dead ++ funcs_and_ids)
  )


(*****************************************************************************)
(* Patch generation helpers *)
(*****************************************************************************)
    
(* 
 * Analyze which lines to remove. Try also to remove associated comments.
 * 
 * Before I was eating too much tokens. 
 *   
 *  bad: let (min, max) = db.Db.defs.Db.range_of_ast#assoc id in 
 * 
 * This is because some whitespaces like newlines are associated
 * with the entity and they sometimes are on the same line that
 * some '}',
 * 
 * So first get ii of ast, then process tokens
 * until first ii, filter space, and look if comment,
 * then take this comment too.
 * check if first column!! or nobody else on its line
 *
 * Update: we dont want to use the lines of comment removed to take
 * a decision about the blamer. So now returns two set of lines, 
 * the code lines and code-and-comment lines. For instance sgrimm
 * was blamed for some deadcode where he actually only modified
 * the comment (and in quite mechanical way); better to blame
 * the person who wrote the code when it comes to deadcode removal.
 * 
 * bugfix: sometimes in facebook they add 2 comments, as in
 *  /**
 *   * Determine if an actor's whitelist has a specific recipient in it.
 *   */
 *  // memcache-refactor: remove this function (check if called anywhere still)
 * 
 * so have to handle this special case too.
 *)
let get_lines_to_remove db ids =
  ids +> List.map (fun (s, fullid, id) ->
    let ast = Db.ast_of_id id db  in

    let toks = Db.toks_of_topid_of_id id db in

    let ii = Lib_parsing_php.ii_of_any (Entity ast) in
    let (min, max) = Parse_info.min_max_ii_by_pos ii in

    let min = PI.parse_info_of_info min in
    let max = PI.parse_info_of_info max in

    let toks_before_min = 
      toks +> List.filter (fun tok ->
        Token_helpers_php.pos_of_tok tok < min.Parse_info.charpos
      ) +> List.rev
    in
    (* note that the tokens in toks_before_min are in reverse order.
     * note also that for one-liner comment the tokens contain
     * the newline. so //comment\nfunctionfoo will return 2 tokens,
     * [T_COMMENT("//comment\n"); T_FUNCTION(...)].
     *)
    let min_comment = 
      match toks_before_min with
      | Parser_php.TNewline i1::
          (Parser_php.T_COMMENT i2|Parser_php.T_DOC_COMMENT i2)::xs ->
          if Ast_php.col_of_info i2 = 0 &&
             (* bugfix: dont want comment far away *)
             Ast_php.line_of_info i1 = min.Parse_info.line - 1
          then PI.parse_info_of_info i2
          else min

      (* bugfix *)
      | Parser_php.T_COMMENT i1first::
          Parser_php.TNewline i1::
          (Parser_php.T_COMMENT i2|Parser_php.T_DOC_COMMENT i2)::xs
        ->
          if Ast_php.col_of_info i1first = 0 &&
             Ast_php.col_of_info i2 = 0
          then PI.parse_info_of_info i2
          else min

      (* for one-liner comment, there is no newline token before *)
      | Parser_php.T_COMMENT i2::xs ->
          if Ast_php.col_of_info i2 = 0
          then PI.parse_info_of_info i2
          else min

      | Parser_php.TNewline i1::tok2::xs ->
          let _ltok2 = Token_helpers_php.line_of_tok tok2 in
          let _lwhite = Ast_php.line_of_info i1 in
          if Ast_php.col_of_info i1 = 0 (* buggy: lwhite <> ltok2 *)
          then (* safe to remove the previous newline too *)
            PI.parse_info_of_info i1
          else min
      | _ -> min
    in
    
    let minline = min.Parse_info.line in
    let maxline = max.Parse_info.line in
    let minline_comment = min_comment.Parse_info.line in
    { just_code_lines = Common2.enum minline maxline;
      code_and_comment_lines = Common2.enum minline_comment maxline
    }
  ) 
  +> (fun couples -> 
    let just_code = couples +> List.map (fun x -> x.just_code_lines) in
    let all_lines = couples +> List.map (fun x -> x.code_and_comment_lines) in
    { just_code_lines        = just_code +> List.flatten;
      code_and_comment_lines = all_lines +> List.flatten;
    })
     
(*****************************************************************************)
(* Deadcode patches *)
(*****************************************************************************)

let generate_deadcode_patch ~prj_path ~filename ~filename_in_project 
    ~hooks ~lines_to_remove xs' =

  let blame = 
    let blamers = 
      Git.get_2_best_blamers_of_lines 
        ~basedir:prj_path 
        ~use_cache:hooks.cache_git_blame
        ~is_valid_author:hooks.is_valid_author
        ~skip_revs:hooks.skip_revs
        filename_in_project 
        lines_to_remove.just_code_lines
    in
    match blamers with
    | [a;b] -> Common.join "_" [a;b]
    | [a] -> 
      pr2 "Just found a single author :(";
      a
    | [] -> "NOBODYTOBLAME"
    | _ -> failwith "Impossible: can get maximum of 2 authors"
  in


  (* 
   * We dont want to generate patches for very recent code, because 
   * people may actually add code that is not yet called but will in the futur.
   * Moreover recent code changes so our built database and patches
   * may not apply anymore once the reviewer get the email.
   * Finally we would prefer in a first round to generate patches only
   * for old code, as it has more chance to be true deadcode.
   * 
   * For all those reasons, it is useful to know the "date" of the patch
   * we will generate.
   * 
   * For now we use the date of the lines of code we will remove.
   * todo? we could use instead the lines of the whole file ?
   * 
   *)

  let date = 
    Git.max_date_of_lines
      ~basedir:prj_path
      ~use_cache:hooks.cache_git_blame
      ~skip_revs:hooks.skip_revs
      filename_in_project
      lines_to_remove.just_code_lines
  in

  let (dir, base) = Common2.db_of_filename filename_in_project in
  let finaldir = Filename.concat hooks.patches_path dir in
  pr2(spf "Blaming %s for file %s" blame filename_in_project);
  Common.command2("mkdir -p " ^ finaldir);

  let patchfile = spf "%s/%s__%s__%s.patch" 
    finaldir
    blame 
    (Common2.string_of_date_dmy date)
    base
  in
  pr2 ("Generating: " ^ patchfile);
  Common2.uncat xs' patchfile;
  ()




(* assume the filename of the patch was generated by function above *)
let deadcode_patch_info patch =
  let base = Filename.basename patch in
  if base =~ "\\(.+\\)__\\(.+\\)__.*" 
  then 
    let (blame, date) = Common.matched2 base in

    let blamers = Common.split "_" blame in
    let (reviewer, cc) = 
      match blamers with
      | ["NOBODYTOBLAME"] -> None, None
      | [x] -> Some x, None
      | [x;y] -> Some x, Some y
      | _ -> raise Impossible
    in
    let first_line = List.hd (Common.cat patch) in
    (* e.g. "--- a/flib/gender/gender.php\t" *)
    if (first_line =~ "--- a/\\([^ \t\n]+\\)")
    then
      let file = Common.matched1 first_line in
      { 
        file = file;
        reviewer = reviewer;
        cc = cc;
        date = Common2.date_dmy_of_string date;
      }
    else 
      failwith ("WIERD: content of deadcode patch seems invalid: " ^ first_line)
  else 
    failwith ("WIERD: the filename does not respect the convention: " ^ patch)

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

(* who has the most deadcode :) *)
let deadcode_stat dir = 
  let files = 
    Common2.files_of_dir_or_files_no_vcs_post_filter "" [dir ^ "/"] in
  let h = Hashtbl.create 101 in

  files +> List.iter (fun file ->

    let info = deadcode_patch_info file in
    let nblines = Common.cat file +> List.length in
    let author = Common2.some_or info.reviewer "NOBODYTOBLAME" in
    Common2.hupdate_default author (fun old -> old + nblines) 
      Common2.cst_zero  h
  
  (* old: Common.command2(spf "mv \"%s\" \"%s.patch\" " file file); *)
  );
  let xs = Common.hash_to_list h in
  let sorted = Common.sort_by_val_highfirst xs in
  sorted +> List.iter (fun (author, count) ->
    pr2(spf "%15s = %d lines of dead code" author count);
  );
  let total = sorted +> List.map snd +> Common2.sum_int in
  pr2 (spf "total = %d" total);
  ()





(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let deadcode_analysis hooks db = 

  if hooks.with_blame then begin
    if not (Common2.command2_y_or_no("rm -rf " ^ hooks.patches_path))
    then failwith "ok we stop";
    Common.command2("mkdir -p " ^ hooks.patches_path);
  end;


  let dead_ids = finding_dead_functions hooks db in

  let prj_path = Db.path_of_project db.Db.project in
  
  (* grouping, blaming, and generate diffs *)

  (* can remove that code below, because most of the time
   * it's because tbgs/glimpse is not aware of diffs between
   * foocall and $foocall or diffs between foocall and foocall_extra_stuff.
   * Test tbgs on 'jobs_get_mba_schools' and it will return also 
   * files having 'jobs_get_mba_schools_names'
   * 
   * This reduces the LOC of deadcode patches from 72000 to 52000
   *)
  (*
  let grouped_by_file = 
    if false 

    grouped_by_file +> List.filter (fun (file, ids) ->
      if ids +> List.exists (fun (s, fullid, id) ->
        let nbmatch = List.length (DbQ.glimpse_get_matching_files s db) in
        if not (nbmatch >= 1)
        then pr2_once "Have you run -index_glimpse ? ";

        if nbmatch > 1
        then pr2("tbgs/glimpse return multiple matches for: " ^ s);
        nbmatch > 1
      )
      then begin
        pr2("so for the moment ignoring file: " ^ file);
        false
      end
      else 
        true
    )
  in
  *)
  let grouped_by_file = 
    deadcode_fixpoint_per_file
      (dead_ids +> List.map snd) 
      hooks db
  in
  let pbs = ref [] in

  grouped_by_file +> List.iter (fun (filename, ids) ->
  try 
   if hooks.skip_patch_generation_for_file filename then ()
   else begin
    
   let lines_to_remove = 
     get_lines_to_remove db ids  in
   let index_lines_to_remove = 
     lines_to_remove.code_and_comment_lines in

   let filename_in_project = 
     Common.filename_without_leading_path prj_path filename in

   let xs' = 
     Patch.generate_patch [Patch.RemoveLines index_lines_to_remove]
       ~filename_in_project filename 
   in

    (* generating patch *)
    if hooks.with_blame 
    then generate_deadcode_patch ~prj_path ~filename ~filename_in_project 
      ~hooks ~lines_to_remove xs'
    else 
      if hooks.print_diff then xs' +> List.iter pr
   end
   with 
   | Timeout -> raise Timeout
   | UnixExit n -> raise (UnixExit n)
   | exn -> 
       let err = spf "PB with %s, exn = %s" filename (Common.exn_to_s exn) in
       pr2 err; Common.push2 err pbs;
  );
  !pbs +> List.iter pr2;
  ()

(*e: deadcode_php.ml *)
