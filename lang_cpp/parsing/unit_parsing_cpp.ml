open Common
open OUnit

open Ast_cpp
module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let parse file = 
  Common.save_excursion Flag.error_recovery false (fun () ->
  Common.save_excursion Flag.show_parsing_error false (fun () ->
    Parse_cpp.parse file
  ))
(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_cpp" >::: [

    (*-----------------------------------------------------------------------*)
    (* Lexing *)
    (*-----------------------------------------------------------------------*)
    (* todo: 
     * - make sure parse int correctly, and float, and that actually does
     *   not return multiple tokens for 42.42
     * - ...
     *)

    (*-----------------------------------------------------------------------*)
    (* Parsing *)
    (*-----------------------------------------------------------------------*)
    "regression files" >:: (fun () ->
      let dir = Filename.concat Config.path "/tests/cpp/parsing" in
      let files = 
        Common.glob (spf "%s/*.cpp" dir) ++ Common.glob (spf "%s/*.h" dir) in
      files +> List.iter (fun file ->
        try
          let _ast = Parse_cpp.parse_program file in
          ()
        with Parse_cpp.Parse_error _ ->
          assert_failure (spf "it should correctly parse %s" file)
      )
    );

    (*-----------------------------------------------------------------------*)
    (* Misc *)
    (*-----------------------------------------------------------------------*)
  ]
