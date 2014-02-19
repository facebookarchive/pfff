(* Yoann Padioleau
 * 
 * Copyright (C) 2009 Yoann Padioleau
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
open Common

module Date = Common2
open Lib_vcs 

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types, globals *)
(*****************************************************************************)

let ext_git_annot_cache = ".git_annot"

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag_version_control.verbose

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this may loop forever ... better to realpath +> split "/" and then
 * process I think. Also dangerous. I think it would be good
 * also when walking the parents to check if there is a .svn or .hg
 * and whatever and then raise an exception
 * 
 * let parent_path_with_dotgit_opt subdir = 
 * let subdir = Common.relative_to_absolute subdir in
 * let rec aux subdir =
 * if Sys.file_exists (Filename.concat subdir "/.git")
 * then Some subdir
 * else 
 * let parent = Common.dirname subdir in
 * if parent = "/"
 * then None
 * else aux parent
 * in
 * aux subdir
 * 
 * let parent_path_with_dotgit a = 
 * Common.some (parent_path_with_dotgit_opt a)
 * 
 * todo: walking of the parent (subject to GIT_CEILING_DIRS)
 *)
let is_git_repository basedir =
  Version_control.detect_vcs_source_tree basedir =*= Some (Version_control.Git)

let find_root_from_absolute_path file =
  let xs = Common.split "/" file in
  let xxs = Common2.inits xs in
  xxs +> List.rev +> Common.find_some (fun xs ->
    let dir = "/" ^ Common.join "/" xs in
    let gitdir = Filename.concat dir ".git" in
    if Sys.file_exists gitdir
    then Some dir
    else None
  )

let cleanup_cache_files dir = 
  let cache_ext = [ext_git_annot_cache] in
  cache_ext +> List.iter (fun ext -> 
    let files = Common2.files_of_dir_or_files_no_vcs ext [dir] in
    files +> List.iter (fun file -> 
      assert(Common2.filesuffix file = ext);
      pr2 file;
      Common.command2(spf "rm -f %s" file);
    ));
  ()

let clean_git_patch xs =
  xs +> Common.exclude (fun s -> 
    s =~ "^index[ \t]" ||
    s =~ "^deleted file mode" ||
    s =~ "^new file mode" ||
    s =~ "^old mode" ||
    s =~ "^new mode" ||
    s =~ ".*No newline at end of file" ||
    false
  )

let exec_cmd ~basedir s =
  let cmd = Lib_vcs.goto_dir basedir^ s in
  pr2 (spf "executing: %s" s);
  let ret = Sys.command cmd in
  if (ret <> 0) 
  then failwith ("pb with command: " ^ s)

(*****************************************************************************)
(* Single file operations, "command output binding" *)
(*****************************************************************************)
(* ex: 
e7ff626d	(Linus Torvalds	2004-09-23 18:49:25 -0700	1)/*
^9a235ca        (     pad       2009-11-21 15:50:04 -0800  1) <?php

can use -M and -C to use better tracking algorithm, can detect
move of lines in same file or accross file in same commit.
does git annotate follow rename ?

*)
let annotate_regexp = 
  "^\\([A-Za-z0-9]+\\)[ \t]+" ^ "([ \t]*\\(.*\\)" ^ "[ \t]+" ^ 
  "\\([0-9]+\\)" ^ "-" ^
  "\\([0-9]+\\)" ^ "-" ^
  "\\([0-9]+\\)" ^ "[ \t]" ^ 
  "[0-9]+" ^ ":" ^
  "[0-9]+" ^ ":" ^
  "[0-9]+" ^ "[ \t]" ^ 
  "[-+]" ^ "[0-9]+" ^ "[ \t]+" ^ 
  "[0-9]+" ^ ")" ^
  ".*$" (* the rest is line of code *)


(* related?  git blame and git pickaxe ? *)
let annotate2 ?(basedir="") ?(use_cache=false) ?(use_dash_C=true) filename = 

  let full_filename = Filename.concat basedir filename in

  (* git blame is really slow, so cache its result *)
  Common.cache_computation ~use_cache full_filename ext_git_annot_cache (fun()->

    (* adding -C leads to better information 
     * adding HEAD so that can get the full information of a file that
     * has been modified in the working tree.
     *)
    let cmd = (goto_dir basedir ^ 
                  spf "git annotate %s HEAD -- \"%s\" 2>&1"
                  (if use_dash_C then "-C" else "")
                  filename)
    in
    (* pr2 cmd; *)
    (* todo? check status. can have a file not under git in which case we
     * get a 'fatal: no such path ... in HEAD
     *)
    let (xs, _status) = Common2.cmd_to_list_and_status cmd in
    (*let ys = Common.cat (Common.filename_of_db (basedir,filename)) in*)

    let annots = 
      xs +> Common.map_filter (fun s -> 
        if s =~ annotate_regexp 
        then 
          let (commitid, author, year, month, day) = Common.matched5 s in
          Some (VersionId commitid, 
               Author author,
               Common2.mk_date_dmy (s_to_i day) (s_to_i month) (s_to_i year))
        else begin 
          pr2 ("git annotate wrong line: " ^ s);
          None
        end
      ) 
    in
    (* files lines are 1_based, so add this dummy 0 entry *)
    Array.of_list (dummy_annotation::annots)
  )
  
let annotate ?basedir ?use_cache ?use_dash_C a = 
  Common.profile_code "Git.annotate" (fun () -> 
    annotate2 ?basedir ?use_cache ?use_dash_C a)

(* ------------------------------------------------------------------------ *)
let annotate_raw ?(basedir="") filename = 

  let cmd = (goto_dir basedir ^ "git annotate HEAD -- "^filename^" 2>&1") in
  (* pr2 cmd; *)
  let xs = Common.cmd_to_list cmd in
  (*let ys = Common.cat (Common.filename_of_db (basedir,filename)) in*)

  let annots = 
    xs +> Common.map_filter (fun s -> 
      if s =~ annotate_regexp 
      then 
        Some s
      else begin 
        (* pr2 ("git annotate wrong line: " ^ s); *)
        None
      end
    ) 
  in
  (* files lines are 1_based, so add this dummy 0 entry *)
  Array.of_list (""::annots)


(* ------------------------------------------------------------------------ *)
(* ex:
Sat, 31 Dec 2005 15:21:18 +0800
*)

let date_regexp = 
  "[A-Za-z]+," ^ "[ \t]+" ^
  "\\([0-9]+\\)" ^ "[ \t]+" ^
  "\\([A-Za-z]+\\)" ^ "[ \t]+" ^
  "\\([0-9]+\\)" ^ "[ \t]+" ^
  ".*$"

let date_file_creation2 ?(basedir="") file = 
  (* note: can't use -1 with git log cos it will show only 1 entry, but
   * the last one, despite the use of --reverse
   *)
  let cmd = (goto_dir basedir ^ 
             "git log --reverse --pretty=format:%aD "^file^" 2>&1")
  in
  (* pr2 cmd; *)
  let xs = Common.cmd_to_list cmd in
  match xs with
  | s::_xs -> 
      if s =~ date_regexp
      then
        let (day, month_str, year) = matched3 s in
        Date.DMY (Date.Day (s_to_i day),
             Common2.month_of_string month_str,
            Date.Year (s_to_i year)
        )
      else failwith ("git log wrong line: " ^ s)
  | _ -> 
      failwith ("git log wrong output")


let date_file_creation ?basedir a = 
  Common.profile_code "Git.date_file" (fun() -> date_file_creation2 ?basedir a)

(*****************************************************************************)
(* Repository operations *)
(*****************************************************************************)

let branches ~basedir = 
  let cmd = (goto_dir basedir ^
                "git branch --no-color") in
  let xs = Common.cmd_to_list cmd in
  xs +> List.map (fun s ->
    if s=~ "[ \t]*\\*[ \t]+\\(.*\\)"
    then matched1 s
    else if s =~ "[ \t]+\\(.*\\)"
    then matched1 s
    else failwith ("wrong line in git branch: " ^ s)
  )


let id_and_summary_oneline s = 
  if s=~ "\\([^ ]+\\) \\(.*\\)"
  then 
    let (commit, summary) = Common.matched2 s in
    VersionId commit, summary
  else
    failwith ("wrong line in git log: " ^ s)
      
let commits ?(extra_args="") ~basedir () = 
  let cmd = (goto_dir basedir ^
                (spf "git log --no-color --pretty=oneline %s" extra_args)) in
  let xs = Common.cmd_to_list cmd in
  xs +> List.map id_and_summary_oneline

let grep ~basedir str =
  let cmd = (goto_dir basedir ^
               (spf "git grep --files-with-matches %s" str)) in
  let (xs, status) = Common2.cmd_to_list_and_status cmd in
  (* According to git grep man page, non-zero exit code is expected when
   * there are no matches
   *)
  match xs, status with
  | [], Unix.WEXITED 1 -> []
  | xs, Unix.WEXITED 0 -> xs
  | _ -> 
    raise (CmdError (status, (spf "CMD = %s, RESULT = %s"
                                cmd (String.concat "\n" xs))))

let show ~basedir file commitid =
  let tmpfile = Common.new_temp_file "git_show" ".cat" in
  let str_commit = Lib_vcs.s_of_versionid commitid in
  let cmd = (spf "git show %s:%s > %s" str_commit file tmpfile) in
  exec_cmd ~basedir cmd;
  tmpfile
  
(*****************************************************************************)
(* single commit operations  *)
(*****************************************************************************)

let commit_raw_patch ~basedir commitid = 
  let (VersionId scommit) = commitid in
  let cmd = (goto_dir basedir ^
             (spf "git show --no-color %s" scommit)) in
  let xs = Common.cmd_to_list cmd in
  xs

let commit_summary ~basedir commitid = 
  let (VersionId scommit) = commitid in
  let cmd = (goto_dir basedir ^
             (* (spf "git show --no-color --pretty=oneline %s" scommit)) in *)
             (spf "git log --pretty=oneline -1 %s" scommit)) in
  let xs = Common.cmd_to_list cmd in
  List.hd xs +> id_and_summary_oneline +> snd

let commit_info ~basedir commitid = 
  let (VersionId scommit) = commitid in
  let cmd = (goto_dir basedir ^
             (* (spf "git show --no-color --pretty=oneline %s" scommit)) in *)
             (spf "git log --format='%%b' -1 %s" scommit)) in
  let xs = Common.cmd_to_list cmd in
  xs

let commit_patch ~basedir commitid = 
  let (VersionId scommit) = commitid in
  let cmd = (goto_dir basedir ^
             (spf "git show --no-color %s" scommit)) in
  let xs = Common.cmd_to_list cmd in
  let xs = clean_git_patch xs in

  Lib_vcs.parse_commit_patch xs


let commit_of_relative_time ~basedir relative_data_string = 
  let cmd = (goto_dir basedir ^
             (spf "git log --no-color --pretty=oneline --since=\"%s\"" 
                 relative_data_string
             )) in
  let xs = Common.cmd_to_list cmd in
  let last = Common2.list_last xs in
  id_and_summary_oneline last +> fst

let files_involved_in_diff ~basedir commitid =
  let str_commit = Lib_vcs.s_of_versionid commitid in
  let cmd = goto_dir basedir ^
    spf "git show --name-status --pretty=\"format:\" %s" str_commit in
  let xs = Common.cmd_to_list cmd in

  assert(List.hd xs = "");
  (* the previous command has a first empty line before the list of files *)
  List.tl xs +> List.map Lib_vcs.parse_file_status

(*****************************************************************************)
(* multiple commits operations  *)
(*****************************************************************************)

let commits_between_commitids ~basedir ~old_id ~recent_id =
  let cmd = (goto_dir basedir ^
             (spf "git log --no-color --pretty=oneline %s..%s"
                 (s_of_versionid old_id)
                 (s_of_versionid recent_id)
             )) in
  let xs = Common.cmd_to_list cmd in
  xs +> List.map id_and_summary_oneline +> List.map fst +> List.rev
 

let file_to_commits ~basedir commits = 
  let h = Common2.hash_with_default (fun() -> []) in
  let total = List.length commits in
  commits +> Common.index_list_1 +> List.iter (fun (vid, cnt) ->
    Common2.log2 (spf "patch %d/%d" cnt total);
    try 
      let patch = commit_patch ~basedir vid in
      let (_strs, patchinfo) = patch in
      
      patchinfo +> List.iter (fun (filename, fileinfo) ->
        (* TODO use fileinfo *)
        h#update filename (fun old -> (vid, fileinfo)::old)
      );
    with e -> 
      pr2 (spf "PB with patch: %s, exn = %s" 
              (Lib_vcs.s_of_versionid vid)
              (Common.exn_to_s e)
      );
      (* TODO *)
  );
  h#to_list

(* very useful when have to send automatic diffs to people, to not penalize
 * the people who have just refactored the code and are actually not really
 * responsible for the code in the file.
 *)
let refactoring_commits ?(since="--since='1 year ago'") ?(threshold=50) repo =
  let basedir = Common.realpath repo in
  let commits = commits ~basedir ~extra_args:since () in
  pr2 (spf "#commits = %d" (List.length commits));
  
  let refactoring_ids = 
  commits +> Console.progress (fun k -> List.filter (fun (id, _x) ->
    k ();
    let (Lib_vcs.VersionId scommit) = id in
    let cmd = (spf "cd %s; git show --oneline --no-color --stat %s"
                  basedir scommit) in
    let xs = Common.cmd_to_list cmd in
    (* basic heuristic: more than N files in a diff => refactoring diff *)
    List.length xs > threshold
  ))
  in
  let tmpfile = "/tmp/refactoring_diffs.list" in
  pr2 (spf "writing data in %s" tmpfile);
  Common.with_open_outfile tmpfile (fun (pr, _chan) ->
    refactoring_ids +> List.iter (fun (id, s) ->
      pr2_gen (id, s);
      pr (spf "%s %s\n" (Lib_vcs.s_of_versionid id) s);
    );
  );
  ()

let parse_skip_revs_file file =
  file +> Common.cat +> List.map (fun s ->
    if s =~ "^\\([^ ]+\\) "
    (* git annotate returns commitid of length 8, so must match that *)
    then Lib_vcs.VersionId (String.sub (Common.matched1 s) 0 8)
    else failwith ("wrong entry in skiprevs file: " ^ s)
  )

(*****************************************************************************)
(* line level operations, preparing commits *)
(*****************************************************************************)

let apply_patch ~basedir patch_string_list = 
  let tmpfile = Common.new_temp_file "git" ".patch" in
  let s = Common2.unlines patch_string_list in
  Common.write_file ~file:tmpfile s;
  
  let cmd = (goto_dir basedir ^ "git apply "^tmpfile^" 2>&1") in
  let xs = Common.cmd_to_list cmd in
  xs +> List.iter pr2;
  ()

(* ------------------------------------------------------------------------ *)
(* 
 * Return which person(s) to blame for some deadcode (in fact certain lines).
 * Do majority, except a whitelist, and if nothing found then
 * do majority of file, and if nothing found (because of whitelist)
 * then say "NOBODYTOBLAME"
 * 
 * One improvement suggested by sgrimm is to use git annotate -C (or 
 * git blame -C) which tries to detect move of code and give a more
 * accurate author. See h_version-control/git.ml.
 * 
 * For instance on www/lib/common.php, 
 * git annotate -C   vs git annotate gives:
 * 
 * 138,147c138,147
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	138) *
 * < d6106bdb	(  jwiseman	2007-07-05 21:58:37 +0000	139) * @param  int       $id       the id of a user or an object
 * < d6106bdb	(  jwiseman	2007-07-05 21:58:37 +0000	140) * @param  string    $exit_fn  the function to call when the connection fails
 * < d6106bdb	(  jwiseman	2007-07-05 21:58:37 +0000	141) * @param  array     $args     arguments to $exit_fn
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	142) * @return resource  a write connection to the specified db
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	143) * @author jwiseman
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	144) */
 * < d6106bdb	(  jwiseman	2007-07-05 21:58:37 +0000	145)function require_write_conn($id, $exit_fn='go_home', $args=null) {
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	146)  $conn_w = id_get_conn($id, 'w');
 * < 2ea63cc5	(  jwiseman	2007-07-03 01:39:41 +0000	147)  if (!$conn_w) {
 * ---
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	138) *
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	139) * @param  int       $id       the id of a user or an object
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	140) * @param  string    $exit_fn  the function to call when the connection fails
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	141) * @param  array     $args     arguments to $exit_fn
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	142) * @return resource  a write connection to the specified db
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	143) * @author jwiseman
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	144) */
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	145)function require_write_conn($id, $exit_fn='go_home', $args=null) {
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	146)  $conn_w = id_get_conn($id, 'w');
 * > effa6f73	(    mcslee	2007-10-18 06:43:09 +0000	147)  if (!$conn_w) {
 * 
 * It is clear that the first series of blame is better, as 
 * it contains multiple commits, and because mcslee was probably just
 * moving code around and not actually modifying the code.
 * 
 * Note that by default git blame does already some analysis such as
 * detecting renaming of files. But it does not do more than that. For 
 * intra files moves, you want git annotate -C.
 * 
 * With -C it takes 130min to run the deadcode analysis on www.
 * Fortunately once it's cached, it takes only 2 minutes.
 * 
 *)
let get_2_best_blamers_of_lines
    ~basedir 
    ?use_cache 
    ?(is_valid_author=(fun _ -> true))
    ?(skip_revs=[])
    filename 
    lines_to_remove 
  =
  (* git blame is really slow, so useful to cache its result *)
  let annots = annotate ~basedir ?use_cache filename in

  let toblame = 
    lines_to_remove +> Common.map_filter (fun i ->
      let (version, Lib_vcs.Author author, _date) = annots.(i) in
      (* todo: commitid string sometimes are specified by their full
       * length, somtimes only by its first 8 characters. Maybe should
       * have a commitid_equal and use that. Right now
       * I assume the skip_revs contain just like the result from
       * git annotate 8-chars commit ids
       *)
      if is_valid_author author && not (List.mem version skip_revs)
      then Some author
      else None
    )
  in

  let hblame = Common.hashset_of_list toblame in
  let other_authors = 
    annots +> Array.to_list +> Common.map_filter (fun x ->
      let (version, Lib_vcs.Author author, _date) = x in
      if is_valid_author author 
         && not (Common2.hmem author hblame) 
         && not (List.mem version skip_revs)
      then Some author
      else None
    )
  in
      
  let counts = Common2.count_elements_sorted_highfirst toblame +>
    List.map fst in
  let counts' = Common2.count_elements_sorted_highfirst other_authors +>
    List.map fst in

  Common2.take_safe 2 (counts @ counts')


let max_date_of_lines ~basedir ?use_cache ?(skip_revs=[])
  filename lines_to_remove =

  let annots = annotate ~basedir ?use_cache filename in

  (* todo? use only the lines_to_remove or the whole file to
   * decide of the "date" of the patch ? *)
  let toblame = 
    lines_to_remove +> Common.map_filter (fun i -> 
      let (version, Lib_vcs.Author _author, date) = annots.(i) in
      if not (List.mem version skip_revs)
      then Some date
      else None
    )
  in
  Common2.maximum_dmy toblame

(*****************************************************************************)
(* Archeology *)
(*****************************************************************************)

(* src:
 * http://www.bramschoenmakers.nl/en/node/645
 * 
 * Sometimes it's handy to checkout a branch based on a point in time but:
 * 
 *   $ git checkout master@{2009-07-27 13:37}
 * 
 * will not work, because it uses the reflog (which expires after some time).
 * 
 * The trick (as found on Nabble) is to lookup the revision on a certain
 * date and check out that revision. This can be done in a single command:
 * 
 *   $ git checkout `git rev-list -n 1 --before="2009-07-27 13:37" master`
 *)
