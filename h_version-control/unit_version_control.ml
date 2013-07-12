open Common

open OUnit

module Lib = Lib_vcs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "version_control" >::: [
    "parsing file status" >:: (fun () ->
      assert_equal
        (Lib.Added, "a/b/foo.php")
        (Lib.parse_file_status "A\ta/b/foo.php");

      assert_equal
        (Lib.Renamed (99, "a/b/foo.php"), "a/c/bar.php")
        (Lib.parse_file_status "R099\ta/b/foo.php\ta/c/bar.php");

    );
    "find root" >:: (fun () ->
      let pfff_home = Config_pfff.path in
      try
        assert_equal
          (Git.find_root_from_absolute_path pfff_home)
          pfff_home;
        assert_equal
          (Git.find_root_from_absolute_path (pfff_home ^ "/"))
          pfff_home;
        assert_equal
          (Git.find_root_from_absolute_path
             (pfff_home ^ "/h_version-control/unit_version_control.ml"))
          pfff_home;
        assert_raises Not_found
          (fun () -> Git.find_root_from_absolute_path "/");
      with Not_found ->
        assert_failure "fail to find root for pfff_home";
    );
  ]
