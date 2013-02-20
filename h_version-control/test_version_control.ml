open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_git dir = 
  let xs = Git.commits ~basedir:dir () in
  xs +> List.iter pr2_gen


(* ------------------------------------------------------------------------ *)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-test_git", "   <dir>", 
  Common.mk_action_1_arg test_git;

  "-test_git2", "   <dir> <str>", 
  Common.mk_action_2_arg (fun basedir str ->
    let vid = 
      (* Git.commit_of_relative_time ~basedir str  *)
      Lib_vcs.VersionId str
    in
    pr2_gen vid;
    let patch = Git.commit_patch ~basedir vid in
    pr2_gen patch;
  );
  "-test_git3", "<dir>", 
  Common.mk_action_1_arg (fun basedir ->

    (* pr2 (Git.parent_path_with_dotgit basedir); *)

    let old_id = Git.commit_of_relative_time ~basedir "2 weeks ago" in
    let recent_id = Git.commit_of_relative_time ~basedir "1 week ago" in
    let commits = Git.commits_between_commitids ~basedir ~old_id ~recent_id in
    pr2_gen old_id;
    pr2_gen recent_id;
    Common2.pr2_xxxxxxxxxxxxxxxxx ();
    commits +> List.iter pr2_gen;
  )
]
