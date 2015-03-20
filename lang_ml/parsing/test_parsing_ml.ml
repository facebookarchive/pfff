open Common

module Ast = Ast_ml
module Flag = Flag_parsing_ml

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_ml file =
  if not (file =~ ".*\\.ml[iyl]?") 
  then pr2 "warning: seems not a ocaml file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  Flag_parsing_ml.exn_when_lexical_error := true;

  let toks = Parse_ml.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_ml_or_mli xs =
  let xs = List.map Common.realpath xs in

  let fullxs = 
    Lib_parsing_ml.find_source_files_of_dir_or_files xs
    +> Skip_code.filter_files_if_skip_list ~verbose:true
  in
  let stat_list = ref [] in

  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
    k();

    let (_xs, stat) = 
      Common.save_excursion Flag_parsing_ml.error_recovery true (fun () ->
        Parse_ml.parse file 
      )
      in
    Common.push stat stat_list;
  ));
  Parse_info.print_parsing_stat_list !stat_list;
  ()

let test_dump_ml file =
  let ast = Parse_ml.parse_program file in
  let v = Meta_ast_ml.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s


let test_parse_ml_fuzzy dir_or_file =
  let fullxs = 
    Lib_parsing_ml.find_source_files_of_dir_or_files [dir_or_file] 
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file -> 
     k ();
      try 
        let _fuzzy = Parse_ml.parse_fuzzy file in
        ()
      with _exn ->
        (* pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn)); *)
        pr2 file;
  ));
  ()

let test_dump_ml_fuzzy file =
  let fuzzy, _toks = Parse_ml.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s

(*****************************************************************************)
(* One shot *)
(*****************************************************************************)

let refactor_grammar subst_file file =
  let h = Hashtbl.create 101 in

  let xs = Common.cat subst_file in
  
  let rec populate_hash xs = 
    match xs with
    | [] -> ()
    | [x] -> failwith ("pb not a pair number: " ^ x)
    | x::y::xs ->
        (if x =~ "\\([A-Za-z]+\\)"
         then 
          let target = Common.matched1 x in
          if y =~ " \\([A-Za-z]+\\)"
          then
            let orig = Common.matched1 y in
            Hashtbl.add h orig target
          else 
            failwith ("wrong format: " ^ x ^ y)
        else 
            failwith ("wrong format: " ^ x ^ y)
        );
        populate_hash xs
  in
  populate_hash xs;

  let ys = Common.cat file in
  ys +> List.iter (fun l ->
    let s = Common2.global_replace_regexp "\\([a-zA-Z_][A-Za-z_0-9]*\\)" (fun s ->
      try 
        Hashtbl.find h s
      with 
      Not_found -> s
    ) l 
    in
    pr s
  );
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_ml", "   <file>", 
  Common.mk_action_1_arg test_tokens_ml;
  "-parse_ml", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_ml_or_mli;
  "-dump_ml", "   <file>", 
  Common.mk_action_1_arg test_dump_ml;

  "-parse_ml_fuzzy", "   <file or dir>", 
  Common.mk_action_1_arg test_parse_ml_fuzzy;
  "-dump_ml_fuzzy", "   <file>", 
  Common.mk_action_1_arg test_dump_ml_fuzzy;

  "-refactor_grammar", "   <subst_file> <file>", 
  Common.mk_action_2_arg refactor_grammar;
]
