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
  "-uninclude_clang", " ",
  Common.mk_action_0_arg (fun () ->
    let skip_file = "skip_list.txt" in
    let skip_list =
      if Sys.file_exists skip_file
      then begin 
        pr2 (spf "Using skip file: %s" skip_file);
        Skip_code.load skip_file
      end
      else []
    in
    Uninclude_clang.uninclude ~verbose:true "." skip_list ".";
  );
  
]
