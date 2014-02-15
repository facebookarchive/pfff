open Common
open OUnit

module Ast = Ast_fuzzy_opa
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
     * - ...
     *)

    (*-----------------------------------------------------------------------*)
    (* Parsing *)
    (*-----------------------------------------------------------------------*)

    "regression files" >:: (fun () ->
      let dir = Filename.concat Config_pfff.path "/tests/opa/parsing" in
      let files = Common2.glob (spf "%s/*.opa" dir) in
      files +> List.iter (fun file ->
        try
          let (_, toks) = Parse_opa.parse_just_tokens file in
          let toks = Ast_fuzzy_opa.toks_for_ast_fuzzy toks in
          let tree = Token_views_opa.mk_tree toks in
          let _tree = Ast_fuzzy_opa.mk_tree tree in
            ()
        with Parse_opa.Parse_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    (*-----------------------------------------------------------------------*)
    (* Misc *)
    (*-----------------------------------------------------------------------*)
  ]
