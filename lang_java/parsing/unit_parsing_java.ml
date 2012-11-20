open Common

open Ast_java
module Ast = Ast_java

open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_java" >::: [

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/java/parsing" in
      let files = Common.glob (spf "%s/*.java" dir) in
      files +> List.iter (fun file ->
        try
          let _ = Parse_java.parse file in
          ()
        with Parse_java.Parse_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );
  ]

