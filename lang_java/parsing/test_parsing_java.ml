(* Copyright (C) 2008 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_java
module PI = Parse_info
module V = Visitor_java

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse xs  =
  let fullxs = Lib_parsing_java.find_source_files_of_dir_or_files xs in
  let ext = "java" in

  let stat_list = ref [] in
  let newscore  = Common2.empty_score () in

  Common2.check_stack_nbfiles (List.length fullxs);

  fullxs +> List.iter (fun file -> 
    pr2 ("PARSING: " ^ file);
    let (_xs, stat) = 
      Common.save_excursion Flag_parsing_java.error_recovery true (fun () ->
        Parse_java.parse file 
      )
    in
    Common.push stat stat_list;
    let s = spf "bad = %d" stat.PI.bad in
    if stat.PI.bad = 0
    then Hashtbl.add newscore file (Common2.Ok)
    else Hashtbl.add newscore file (Common2.Pb s)
    ;
  );
  flush stdout; flush stderr;

  PI.print_parsing_stat_list !stat_list;

  let dirname_opt = 
    match xs with
    | [x] when Common2.is_directory x -> Some (Common.realpath x)
    | _ -> None
  in
  let score_path = Filename.concat Config_pfff.path "tmp" in
  dirname_opt +> Common.do_option (fun dirname -> 
    pr2 "--------------------------------";
    pr2 "regression testing  information";
    pr2 "--------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    Common2.regression_testing newscore 
      (Filename.concat score_path
       ("score_parsing__" ^str ^ ext ^ ".marshalled"))
  );
  ()

let test_lexer file =
  let lexbuf = Lexing.from_channel (open_in file) in
  while true do
    let result = Lexer_java.token lexbuf in
    pr2_gen result;
    if Token_helpers_java.is_eof result then
      exit 0
  done

let test_dump file =
  let ast = Parse_java.parse_program file in
  let v = Meta_ast_java.vof_any (Ast_java.AProgram ast) in
  let str = Ocaml.string_of_v v in
  pr str

let test_visitor file = 
  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) e -> 
      match e with
      | Ast_java.Literal (s,_) -> 
          pr2 ("lit:" ^ s);
          k e
      | Ast_java.Dot (e, (_s,_)) -> 
          pr2 "dot: s";
          k e
      | _ -> k e
    );
  } in

  let ast = Parse_java.parse_program file in
  visitor (AProgram ast);
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-tokens_java", "   <file>", 
  Common.mk_action_1_arg test_lexer;
  "-parse_java", "   <file or dir>", 
  Common.mk_action_n_arg test_parse;
  "-dump_java", "   <file>", 
  Common.mk_action_1_arg test_dump;

  "-visitor_java", "   <file>", 
  Common.mk_action_1_arg test_visitor;
]
