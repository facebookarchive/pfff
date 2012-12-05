open Common

open Ast_cpp
module Ast = Ast_cpp
module Flag = Flag_parsing_cpp
module TH = Token_helpers_cpp

module Stat = Statistics_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cpp file = 
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  let toks = Parse_cpp.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_cpp ?c xs  =
  let fullxs = Lib_parsing_cpp.find_cpp_files_of_dir_or_files xs in

  Parse_cpp.init_defs !Flag.macros_h;

  let stat_list = ref [] in
  let newscore  = Common.empty_score () in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);
    
    let (xs, stat) = Parse_cpp.parse ?c file in

    Common.push2 stat stat_list;

    let s = spf "bad = %d" stat.Stat.bad in
    if stat.Stat.bad = 0
    then Hashtbl.add newscore file (Common.Ok)
    else Hashtbl.add newscore file (Common.Pb s)
  );

  Stat.print_recurring_problematic_tokens !stat_list;
  (match xs with 
  | [dirname] when is_directory dirname ->
      pr2 "--------------------------------";
      pr2 "regression testing  information";
      pr2 "--------------------------------";
      let score_path = Filename.concat !Flag.path "tmp" in
      let str = Str.global_replace (Str.regexp "/") "__" dirname in
      Common.regression_testing newscore 
        (Filename.concat score_path
            ("score_parsing__" ^str ^ "cpp" ^ ".marshalled"));


      let layer_file = "/tmp/layer_parse_errors_red_green.json" in
      pr2 (spf "generating parse error layer in %s" layer_file);
      Layer_parse_errors.gen_red_green_layer ~root:dirname ~output:layer_file
        !stat_list;

      let layer_file = "/tmp/layer_parse_errors_heatmap.json" in
      pr2 (spf "generating parse error layer in %s" layer_file);
      Layer_parse_errors.gen_heatmap_layer ~root:dirname ~output:layer_file
        !stat_list;
  | _ -> ()
  );
  Stat.print_parsing_stat_list !stat_list;
  ()

let test_dump_cpp file =
  Parse_cpp.init_defs !Flag.macros_h;
  let ast = Parse_cpp.parse_program file in
  let s = Export_ast_cpp.ml_pattern_string_of_program ast in
  pr s

let test_dump_cpp_full file =
  Parse_cpp.init_defs !Flag.macros_h;
  let ast = Parse_cpp.parse_program file in
  let toks = Parse_cpp.tokens file in
  let precision = { Meta_ast_generic.
     full_info = true; type_info = false; token_info = true;               
  }
  in
  let s = Export_ast_cpp.ml_pattern_string_of_program ~precision ast in
  pr s;
  toks +> List.iter (fun tok ->
    match tok with
    | Parser_cpp.TComment (ii) ->
        let v = Parse_info.vof_info ii in
        let s = Ocaml.string_of_v v in
        pr s
    | _ -> ()
  );
  ()


let test_view_cpp file =
  let toks = Parse_cpp.tokens file 
    +> Common.exclude Token_helpers_cpp.is_comment in
  let extended = 
    toks +> List.map Token_views_cpp.mk_token_extended in
  Parsing_hacks_cpp.find_template_inf_sup extended;

  let groups = Token_views_cpp.mk_multi extended in
  let s = Token_views_cpp.string_of_multi_grouped groups in
  pr2 s

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
    "-parse_cpp", "   <file or dir>", 
    Common.mk_action_n_arg test_parse_cpp;
    "-tokens_cpp", "   <file>", 
    Common.mk_action_1_arg test_tokens_cpp;
    "-dump_cpp", "   <file>", 
    Common.mk_action_1_arg test_dump_cpp;
    "-dump_cpp_ml", "   <file>", 
    Common.mk_action_1_arg test_dump_cpp;
    "-view_cpp", "   <file>", 
    Common.mk_action_1_arg test_view_cpp;
    "-dump_cpp_full", "   <file>", 
    Common.mk_action_1_arg test_dump_cpp_full;

]
