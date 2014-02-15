
(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
    "-parse_objc", " <file or dir>", 
    Common.mk_action_n_arg (fun xs -> 
      Test_parsing_cpp.test_parse_cpp ~lang:Flag_parsing_cpp.ObjectiveC xs
    );
    "-tokens_objc", " <file>", 
    Common.mk_action_1_arg (fun file -> 
      Test_parsing_cpp.test_tokens_cpp file
    );
    "-dump_objc", " <file>",
    Common.mk_action_1_arg (fun file ->
      Test_parsing_cpp.test_dump_cpp file
    );
]
