open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  "-sanitize_compile_commands", " <>",
  Common.mk_action_1_arg (Compile_commands_clang.sanitize_compile_commands);
  "-analyze_make_trace", " <file>",
  Common.mk_action_1_arg (fun file ->
    let json = Compile_commands_clang.analyze_make_trace file in
    pr (Json_out.string_of_json json)
  );
  "-uninclude_clang", "",
  Common.mk_action_0_arg (fun () ->
    Uninclude_clang.uninclude ~verbose:true "." [] ".";
  );
  
]
