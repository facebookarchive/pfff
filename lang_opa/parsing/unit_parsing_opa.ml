open Common
open OUnit

open Ast_opa
module Ast = Ast_opa
module Flag = Flag_parsing_opa

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_opa" >::: [

    (*-----------------------------------------------------------------------*)
    (* Lexing *)
    (*-----------------------------------------------------------------------*)
    (* todo: 
     * - make sure parse int correctly, and float, and that actually does
     *   not return multiple tokens for 42.42
     * - make sure string interpolation generates multiple tokens
     *)

    (*-----------------------------------------------------------------------*)
    (* Parsing *)
    (*-----------------------------------------------------------------------*)

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config.path "/tests/opa/parsing" in
      let files = Common.glob (spf "%s/*.opa" dir) in
      files +> List.iter (fun file ->
        try
          let _ = Parse_opa.parse_just_tokens file in
          ()
        with Parse_opa.Parse_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    (*-----------------------------------------------------------------------*)
    (* Misc *)
    (*-----------------------------------------------------------------------*)
  ]
