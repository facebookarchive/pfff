open Common

module Stat = Parse_info
(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse_c xs =
  let fullxs = Lib_parsing_c.find_source_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs +> (*Console.progress (fun k -> *) List.iter ((fun file -> 
    (*k(); *)
    pr (spf "PARSING: %s" file);
    let (_xs, stat) = 
      Parse_c.parse file 
    in
    Common.push stat stat_list;
  ));
  Stat.print_recurring_problematic_tokens !stat_list;
  Stat.print_parsing_stat_list !stat_list;
  ()

let test_dump_c file =
  let ast = Parse_c.parse_program file in
  let v = Meta_ast_c.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s
  
(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
    "-parse_c", "   <file or dir>", 
    Common.mk_action_n_arg test_parse_c;
    "-dump_c", "   <file>", 
    Common.mk_action_1_arg test_dump_c;
]
