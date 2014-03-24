open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  "-analyze_make_trace", " <file>",
  Common.mk_action_1_arg (fun file ->
    let json = Compile_commands_clang.analyze_make_trace file in
    pr (Json_out.string_of_json json)
  );
  "-sanitize_compile_commands", " <>",
  Common.mk_action_1_arg (fun file ->
    let json = Json_in.load_json file in
    let json = Compile_commands_clang.sanitize_compile_commands json in
    pr (Json_out.string_of_json json);
  );
  "-uninclude_clang", "<src> ",
  Common.mk_action_1_arg (fun src ->
    let root = Common.realpath src  in
    let files = Lib_parsing_clang.find_source_files_of_dir_or_files [root] in
    Uninclude_clang.uninclude ~verbose:true root files root;
  );
  "-uninclude_clang_debug", "<src> <file> <dst>",
  Common.mk_action_3_arg (fun src file dst ->
    let root = Common.realpath src  in
    let files = [Common.realpath file]  in
    let dst = Common.realpath dst in
    Uninclude_clang.uninclude ~verbose:true root files dst;
  );
  
]
