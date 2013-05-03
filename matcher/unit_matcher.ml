open Common
open OUnit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (_parse_func: (string -> Ast_fuzzy.tree list) ref) = ref 
  (fun s -> failwith "parse_func not defined")

(* there is a circular dependency right now as lang_cpp/ depends on
 * matcher/ and matcher unit tests needs a fuzzy parser and so
 * needs lang_cpp/. At some point we could have a toy fuzzy parser
 * that simply use GenLex. For now we break the ciruclar dependency
 * by using function pointers set in main_sgrep.ml and main_spatch.ml.
 *)
let ast_fuzzy_of_string str =
  !_parse_func str

(*****************************************************************************)
(* Sgrep Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Sgrep *)

(* run by sgrep -test *)
let sgrep_unittest = [
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
let spatch_unittest = [
  "spatch regressions files" >:: (fun () ->

(*  
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

        
        let _pattern = 
          raise Todo
          (* Spatch_php.parse spatchfile in *)
        in
        let resopt = 
          try 
            raise Todo
            (* Spatch_php.spatch pattern phpfile *)
          with Failure s ->
            assert_failure (spf "spatch on %s have resulted in exn = %s"
                               srcfile s)
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
*)
    ()
  )
]

(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)

let unittest =
  "matcher" >::: (
    sgrep_unittest ++ spatch_unittest
  )
