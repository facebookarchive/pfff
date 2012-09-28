open Common
open OUnit

open Ast_php
module Ast = Ast_php
module Flag = Flag_parsing_php

(*****************************************************************************)
(* Sgrep Unit tests *)
(*****************************************************************************)

(* See https://github.com/facebook/pfff/wiki/Sgrep *)

(* run by sgrep -test *)
let sgrep_unittest = [

  "sgrep features" >:: (fun () ->
    (* spec: pattern string, code string (statement), should_match boolean *)
    let triples = [
      (* basic string match of course *)
      "foo(1,2);", "foo(1,2);", true;
      "foo(1,3);", "foo(1,2);", false;
      (* matches even when space differs *)
      "foo(1,2);", "foo(1,     2);", true;

      (* expression metavariable *)
      "foo(X);",  "foo(1);", true;
      "foo(X);",  "foo(1+1);", true;

      (* metavariable naming conventions *)
      "foo(X1);",       "foo(1);", true;
      "foo(X1_MISC);",  "foo(1);", true;
      "foo(X_MISC);",   "foo(1);", true;
      "foo(_MISC);",    "foo(1);", false;

      (* variable metavariable *)
      "foo($X);",  "foo($var);", true;

      (* lvalue metavariable *)
      "$V->method();",  "$this->method();", true;
      "$V->method();",  "$this->foo()->method();", true;
      (* TODO: would be good to have this working too 
      "X->method();"  ,  "$this->foo()->method();", true;
      *)

      (* "linear" patterns, a la Prolog *)
      "X && X;", "($a || $b) && ($a || $b);", true;
      "foo($V, $V);", "foo($x, $x);", true;
      "foo($V, $V);", "foo($x, $y);", false;

      (* '...' in funcall *)
      "foo(...);", "foo();", true;
      "foo(...);", "foo(1);", true;
      "foo(...);", "foo(1,2);", true;
      "foo(X,...);", "foo(1,2);", true;
      (* ... also match when there is no additional arguments *)
      "foo(X,...);", "foo(1);", true;
      (* TODO: foo(..., 3, ...), foo(1,2,3,4) *)

      (* '...' in arrays *)
      "foo(X, array(...));",  "foo(1, array(2, 3));", true;

      (* '...' in strings *)
      "foo(\"...\");", "foo(\"a string\");", true;

      (* many arguments metavariables *)
      "foo(MANYARGS);", "foo(1,2,3);", true;
      "foo(MANYARGS2);", "foo(1,2,3);", true;
      "foo(MANYARGS);", "foo();", true;
      "foo(X, MANYARGS);", "foo(1,2,3);", true;
      (* ... also match when there is no additional arguments *)
      "foo(X, MANYARGS);", "foo(1);", true;

      (* metavariables on function name *)
      "X(1,2);", "foo(1,2);", true;
      (* metavariables on class name *)
      "X::foo();", "Ent::foo();", true;
      (* metavariable string for identifiers *)
      "foo('X');", "foo('a_func');", true;
      (* metavariable on reference arguments *)
      "foo(X,Y);", "foo(&$a, $b);", true;
      
      (* isomorphism on "keyword" arguments *)
      "foo(true);", "foo($x=true);", true;
      "foo(true);", "foo(true);", true;
  
      (* we want sgrep/spatch to be case insensitive, like PHP *)
      "foo(...);", "Foo(true);", true;
      "Foo(...);", "foo(true);", true;
      "foo(...);", "Fo0(true);", false;

      (* more complex expressions *)
      "strstr(...) == false;", "strstr($x)==false;", true;

      (* regexp, PCRE syntax *)
      "foo('=~/.*CONSTANT/');", "foo('MY_CONSTANT');", true;
      "foo('=~/.*CONSTANT/');", "foo('MY_CONSTAN');", false;

      (* statements *)
      "if(X) { foo(); }", "if(true) { foo(); }", true;

      (* ------------ *)
      (* xhp patterns *)
      (* ------------ *)

      (* order does not matter *)
      "return <x:frag border=\"1\" foo=\"2\" ></x:frag>;", 
      "return <x:frag foo=\"2\" border=\"1\" ></x:frag>;", 
      true;
      "return <x:frag border=\"1\" foo=\"2\" ></x:frag>;", 
      "return <x:frag foo=\"3\" border=\"1\" ></x:frag>;", 
      false;

      (* concrete code can have more fields *)
      "return <x:frag border=\"1\"></x:frag>;", 
      "return <x:frag foo=\"2\" border=\"1\" ></x:frag>;", 
      true;

      "return <x:frag />;", "return <x:frag border=\"1\" />;", true;

      (* concrete code can have a body *)
      "return <x:frag border=\"1\"></x:frag>;", 
      "return <x:frag border=\"1\" >this is text</x:frag>;", 
      true;

      (* metavariable on xhp tag *)
      "return <X label=\"1\"></X>;", "return <x:frag label=\"1\"></x:frag>;",
      true;

      (* metavariable on xhp label *)
      "return <X Y=\"1\"></X>;", "return <x:frag label=\"1\"></x:frag>;",
      true;

      (* xhp classes have a different syntax when used in xml context (<tag...)
       * and when used as regular classes (:tag...), but a metavariable should
       * accomodate both syntax
       *)
      "return <X>{X::foo()}</X>;", "return <x:frag>{:x:frag::foo()}</x:frag>;",
      true;
      (* TODO:
       *  Xhp should also match XhpSingleton or optional closing tag
       * "return <x:frag></x:frag>;", "return <x:frag />;", true; 
       * "return <x:frag></x:frag>;", "return <x:frag></>;", true; 
       *)
    ] in
    triples +> List.iter (fun (spattern, scode, should_match) ->
      match Sgrep_php.parse spattern, Parse_php.any_of_string scode with
      | Stmt2 pattern, Stmt2 code ->
          let matches_with_env = Matching_php.match_st_st pattern code in
          if should_match
          then
            assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
              (matches_with_env <> [])
          else
            assert_bool (spf "pattern:|%s| should not match |%s" spattern scode)
              (matches_with_env = [])
    | _ ->
        assert_failure "parsing problem in sgrep pattern parsing"
    )
  );

  "sgrep variable metavars matching" >:: (fun () ->
    let pattern = Parse_php.any_of_string "foo($V, $V);" in
    let code = Parse_php.any_of_string "foo($x, $y);" in
    (match pattern, code with
    | Stmt2 pattern, Stmt2 code ->
        let matches_with_env = Matching_php.match_st_st pattern code in
        assert_bool "it should not match" (matches_with_env = []);
    | _ ->
        assert_failure "parsing problem in sgrep pattern parsing"
    );
  );

  "toplevel sgrep matching" >:: (fun () ->
    (* pattern string, code string *)
    let pairs = [
      "function X(){ return Y(...); }","function foo(){ return bar(); }";
      "function X(...){ return Y(...); }","function foo($x){ return bar(); }";
    ] in
    pairs +> List.iter (fun (spattern, scode) ->
      match Sgrep_php.parse spattern, Parse_php.any_of_string scode with
      | Toplevel pattern, Toplevel code ->
          let matches_with_env = Matching_php.match_top_top pattern code in
          assert_bool (spf "pattern:|%s| should match |%s" spattern scode)
            (matches_with_env <> []);
    | _ ->
        assert_failure "parsing problem in sgrep pattern parsing"
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

    let testdir = Filename.concat Config.path "tests/php/spatch/" in
    let expfiles = Common.glob (testdir ^ "*.exp") in
  
    expfiles +> List.iter (fun expfile ->
      (* todo: this regexp should just be .*? but ocaml regexp do not
       * have the greedy feature :( Also note that expfile is a fullpath
       * so it can contains /, hence this ugly regexp
       *)
      if expfile =~ "\\([a-zA-Z_/]+\\)\\([0-9]*\\)\\.exp$" then begin
        let (prefix, variant) = Common.matched2 expfile in
        let spatchfile = prefix ^ ".spatch" in
        let phpfile = prefix ^ variant ^ ".php" in
        
        let pattern = Spatch_php.parse spatchfile in
        let resopt = 
          try Spatch_php.spatch pattern phpfile 
          with Failure s ->
            assert_failure (spf "spatch on %s have resulted in exn = %s"
                               phpfile s)
        in
        
        let file_res = 
          match resopt with
          | None -> phpfile
          | Some s ->
              let tmpfile = Common.new_temp_file "spatch_test" ".php" in
              Common.write_file ~file:tmpfile s;
              tmpfile
        in
        let diff = Common.unix_diff file_res expfile in
        diff +> List.iter pr;
        if List.length diff > 1
        then assert_failure
          (spf "spatch %s on %s should have resulted in %s" 
              (Filename.basename spatchfile)
              (Filename.basename phpfile)
              (Filename.basename expfile))
      end 
      else failwith ("wrong format for expfile: " ^ expfile)
    )
  )
]
(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)

let unittest =
  "matcher_php" >::: (
    sgrep_unittest ++ spatch_unittest
  )
