open Common

open Ast_ml
module Ast = Ast_ml
module Flag = Flag_parsing_ml

open OUnit

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_ml file = 
  if not (file =~ ".*\\.ml[iyl]?") 
  then pr2 "warning: seems not a ocaml file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_ml.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_parse_ml_or_mli xs =

  let fullxs = Lib_parsing_ml.find_ml_files_of_dir_or_files xs in
  let stat_list = ref [] in

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);

    let (xs, stat) = Parse_ml.parse file in
    Common.push2 stat stat_list;
  );
  Parse_info.print_parsing_stat_list !stat_list;
  ()



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
    let s = Common.global_replace_regexp "\\([a-zA-Z_][A-Za-z_0-9]*\\)" (fun s ->
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
(* Unit tests *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_ml", "   <file>", 
  Common.mk_action_1_arg test_tokens_ml;
  "-parse_ml", "   <files or dirs>", 
  Common.mk_action_n_arg test_parse_ml_or_mli;
  "-refactor_grammar", "   <subst_file> <file>", 
  Common.mk_action_2_arg refactor_grammar;
]
