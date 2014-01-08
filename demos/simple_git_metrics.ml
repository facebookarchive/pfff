open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Bug study: gather a few simple metrics that would help better analyze the
 * repository:
 * - # diffs that have 0-10 lines
 * - # diffs that have 10-100 lines
 * - # diffs that have more than 100 lines
 * - # diffs that have either "bug", "fatal", "bugfix" in the summary
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
  
type log_stat = {
  mutable patch : int;
  mutable short : int;
  mutable mid : int;
  mutable long : int;
  mutable haskeywords : int;
  mutable jiao : int;
}

let init_stat = {
  patch = 0;
  short = 0;
  mid = 0;
  long = 0;
  haskeywords = 0;
  jiao = 0;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pr_stat stat = 
  Printf.printf "patch %d\nshort %d\nmid %d\nlong %d\nbugfix %d\njiao %d\n"
    stat.patch stat.short stat.mid stat.long stat.haskeywords stat.jiao

let id s = 
  if s=~ "\\([^ ]+\\) .*"
  then 
    let commit = matched1 s in
    Lib_vcs.VersionId commit
  else
    failwith ("wrong line in git log: " ^ s)

let commits_since ~basedir ~since =
  let cmd = (Lib_vcs.goto_dir basedir ^
               (spf "git log --no-color --pretty=oneline --since=\"%s\""
                  since)) in
  let xs = cmd_to_list cmd in
  xs +> List.map id +> List.rev
    
let has_keywords_in_header (xs,_) =
  xs +> List.exists (fun s -> (s =~ ".*\\bbug\\b.*") 
    || (s =~ ".*\\bbugfix\\b.*") || (s =~ ".*\\bfatal\\b.*"))

let committed_by_jiao (xs,_) =
  xs +> List.exists (fun s -> (s =~ "Author: Jiao Li <jiaol@fb.com>")
    || (s =~ "Author: jiaol.*"))

let lines (_, pinfo) =
  let flines (_,finfo) =
    let fstat = Patch.diffstat_file finfo in
    fstat.Patch.nb_minus + fstat.Patch.nb_plus
  in
  pinfo +> List.map flines +> List.fold_left (+) 0

let do_metrics stat dir commit =
  try(
    let patch = Git.commit_patch ~basedir:dir commit in
    stat.patch <- stat.patch + 1;
    if has_keywords_in_header patch
    then stat.haskeywords <- stat.haskeywords + 1;
    if committed_by_jiao patch
    then stat.jiao <- stat.jiao + 1;
    let nline = lines patch in
    match () with
    | _ when nline < 10 -> stat.short <- stat.short + 1
    | _ when nline < 100 -> stat.mid <- stat.mid + 1
    | _ -> stat.long <- stat.long + 1)
  with
  | Failure s -> 
    pr2 (spf "PB with commit %s, err = %s"
           (Lib_vcs.s_of_versionid commit) s)
  | Stack_overflow ->
    pr2 (spf "SKIPPING %s, stack overflow"
           (Lib_vcs.s_of_versionid commit))
      
(*****************************************************************************)
(* Main Function and Entry Point *)
(*****************************************************************************)

let simple_metrics dir =
  if not (Sys.is_directory dir) then
    raise (Invalid_argument "not a directory");
  if not (Git.is_git_repository dir) then
    raise (Invalid_argument "not a git repo");
  let stat = init_stat in
  let commits = commits_since 
    ~since:"7 days ago"
    ~basedir:dir
  in
  List.iter (do_metrics stat dir) commits;
  pr_stat stat
    
let _main =
  let dir = Sys.argv.(1) in
  simple_metrics dir
