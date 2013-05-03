open Common
open OUnit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Sgrep Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Sgrep *)

(* run by sgrep -test *)
let sgrep_unittest ~ast_fuzzy_of_string = [
  "sgrep features" >:: (fun () ->

    (* spec: pattern string, code string, should_match boolean *)
    let triples = [
      (* basic string match of course *)
      "foo(1,2);", "foo(1,2);", true;
      "foo(1,3);", "foo(1,2);", false;
      (* matches even when space differs *)
      "foo(1,2);", "foo(1,     2);", true;

    ]
    in
    triples +> List.iter (fun (spattern, scode, should_match) ->
      let pattern = ast_fuzzy_of_string spattern in
      let code = ast_fuzzy_of_string scode in
      let matches_with_env = Matching_fuzzy.match_trees_trees pattern code in
      if should_match
      then
        assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
          (matches_with_env <> [])
      else
        assert_bool (spf "pattern:|%s| should not match |%s" spattern scode)
          (matches_with_env = [])
    )
  );
]

(*****************************************************************************)
(* Spatch Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Spatch *)

(* run by spatch -test *)
let spatch_unittest 
    ~ast_fuzzy_of_string ~parse_file ~elt_of_tok ~info_of_tok = 
  [
  "spatch regressions files" >:: (fun () ->

    let testdir = Filename.concat Config_pfff.path "tests/fuzzy/spatch/" in
    let expfiles = Common2.glob (testdir ^ "*.exp") in

    expfiles +> List.iter (fun expfile ->
      (* todo: this regexp should just be .*? but ocaml regexp do not
       * have the greedy feature :( Also note that expfile is a fullpath
       * so it can contains /, hence this ugly regexp
       *)
      if expfile =~ "\\([a-zA-Z_/]+\\)\\([0-9]*\\)\\.exp$" then begin
        let (prefix, variant) = Common.matched2 expfile in
        let spatchfile = prefix ^ ".spatch" in
        let srcfile = prefix ^ variant ^ ".fuzzy" in

        let pattern =
          Spatch_fuzzy.parse
            ~pattern_of_string:ast_fuzzy_of_string
            ~ii_of_pattern:Ast_fuzzy.ii_of_trees
            spatchfile
        in
        let trees, toks = 
          parse_file srcfile
        in
        let was_modified = Spatch_fuzzy.spatch pattern trees in

        let elts_of_tok tok =
          Lib_unparser.elts_of_any 
            ~elt_of_tok:elt_of_tok
            ~info_of_tok:info_of_tok 
            tok
        in
        let unparse toks = 
          Lib_unparser.string_of_toks_using_transfo ~elts_of_tok toks
        in
        let resopt =
          if was_modified
          then Some (unparse toks)
          else None
        in

        let file_res = 
          match resopt with
          | None -> srcfile
          | Some s ->
            let tmpfile = Common.new_temp_file "spatch_test" ".fuzzy" in
            Common.write_file ~file:tmpfile s;
            tmpfile
        in
        let diff = Common2.unix_diff file_res expfile in
        diff +> List.iter pr;
        if List.length diff > 1
        then assert_failure
          (spf "spatch %s on %s should have resulted in %s" 
              (Filename.basename spatchfile)
              (Filename.basename srcfile)
              (Filename.basename expfile))
      end 
      else failwith ("wrong format for expfile: " ^ expfile)
    )
  )
]

(*****************************************************************************)
(* Misc unit tests *)
(*****************************************************************************)
let misc_unittest =
  "misc" >::: [
    "join_with_space" >:: (fun () ->
      assert_equal
        (Lib_matcher.join_with_space_if_needed ["$x";"=";"print";"FOO"])
        "$x=print FOO"
    )
  ]

(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)

(*
let unittest =
  "matcher" >::: (
    sgrep_unittest ++ spatch_unittest ++ [misc_unittest]
  )
*)
