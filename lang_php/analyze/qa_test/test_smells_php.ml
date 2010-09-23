(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common

open Ast_php

module Ast = Ast_php
module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * For now a "bad smell" for a test is only whether or not it has a bad
 * cyclomatic complexity number. We could do more later.
 * 
 * The algorithm is pretty simple:
 * iterate files, parse, visit, compute cyclo, remember in hash, and rank.
 * 
 * Because it does not require global analysis, I don't use the pfff db, 
 * I parse directly the files.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type selection = 
  | Threshold of int
  | Topn of int

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_bad_cyclo name cyclo = 
  let info = Ast.info_of_name name in
  spf "cyclo for %s at %s:%d = %d" 
    (Ast.name name) 
    (Ast.file_of_info info)
    (Ast.line_of_info info)
    cyclo


let cyclomatic_complexity_file file = 
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let res = ref [] in

  let hooks = { V.default_visitor with
    V.kfunc_def = (fun (k, _) def ->
      (* the function can have nested funcs *)
      k def;
      let flow = Controlflow_build_php.cfg_of_func def in
      let cyclo = Cyclomatic_php.cyclomatic_complexity_flow flow in
      Common.push2  (def.f_name, cyclo) res;

       );
       V.kmethod_def = (fun (k, _) def ->
         (* the method can have nested funcs *)
         k def;
         let flow = Controlflow_build_php.cfg_of_method def in
         let cyclo = Cyclomatic_php.cyclomatic_complexity_flow flow in
         Common.push2  (def.m_name, cyclo) res;
       );
     }
  in
  (try 
      (V.mk_visitor hooks).V.vprogram ast;
    with
    Controlflow_build_php.Error err ->
      Controlflow_build_php.report_error err
  );
  !res
 

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (code_with_bad_cyclomatic: selection -> Common.filename list -> unit) =
 fun selection files ->
   
   let hscore = Hashtbl.create 101 in

   (* populating hscore *)
   Flag_parsing_php.show_parsing_error := true;
   files +> List.iter (fun file ->
     pr2 (spf "processing: %s" file);

     let cyclos = cyclomatic_complexity_file file in
     cyclos +> List.iter (fun (name, cyclo) ->
       Hashtbl.add hscore name cyclo;
       pr2 (string_of_bad_cyclo name cyclo);
     );
   );

   (* rank *)
   let xs = Common.hash_to_list hscore in
   let bad = 
     match selection with
     | Threshold n ->
         xs +> List.filter (fun (name, score) -> 
           score >= n
         ) +> Common.sort_by_val_highfirst
     | Topn n -> 
         xs +> Common.sort_by_val_highfirst +> 
           Common.take_safe n
   in
   pr2_xxxxxxxxxxxxxxxxx ();
   bad +> List.iter (fun (name, score) -> 
     pr2 (string_of_bad_cyclo name score)
   )
   



(*****************************************************************************)
(* Actions *)
(*****************************************************************************)
let actions () = [
  "-test_bad_cyclomatic", " <files_or_dirs>",
  Common.mk_action_n_arg (fun xs ->
    let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
    code_with_bad_cyclomatic (Threshold 10) files
  );

  "-print_cyclomatic", " <files_or_dirs>",
  Common.mk_action_n_arg (fun xs ->
    let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in
    files +> List.iter (fun file ->
      let cyclos = cyclomatic_complexity_file file 
      in
      pr2_gen cyclos;
  ));

  (* see http://www.r-project.org *)
  "-print_cyclomatic_R", " <files_or_dirs>",
  Common.mk_action_n_arg (fun xs ->
    let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

    let all_cyclos = 
    files +> List.map (fun file ->
      let cyclos = cyclomatic_complexity_file file in
      cyclos +> List.map (fun (name, cyclo) -> cyclo)
    ) +> List.flatten
      (* why ? *)
      +> Common.exclude (fun x -> x <= 1)
    in
    let r_cmd = spf "
x = c(%s)
boxplot(x)
" (all_cyclos +> List.map i_to_s +> Common.join ", ")
    in
    pr r_cmd;
  );


  "-compare_cyclomatic_R", " <flaky_tests file> <test dir>",
  Common.mk_action_2_arg (fun flaky_test_file testdir ->

    let flaky_files = Common.cat flaky_test_file in
    let all_files = Lib_parsing_php.find_php_files_of_dir_or_files [testdir] in

    let compute_cyclos files = 
        files +> List.map (fun file ->
          let cyclos = cyclomatic_complexity_file file in
          cyclos +> List.map (fun (name, cyclo) -> cyclo)
        ) +> List.flatten
          (* prefer to filter the too simple trivial cyclomatic func/method
           * from the statistic report
           *)
        +> Common.exclude (fun x -> x <= 1)
    in
    let flaky_cyclos = compute_cyclos flaky_files in
    let all_cyclos   = compute_cyclos all_files in
    
    let r_cmd = spf "
postscript(\"/tmp/flaky_vs_all.ps\")
par(mfrow=c(1, 2))
x1 = c(%s)
x2 = c(%s)
boxplot(x1, x2, names=c(\"flaky tests(%d)\", \"all tests(%d)\"), 
        ylim=c(0,15), ylab=\"cyclomatic complexity of nontrivial func/methods\")

" 
(flaky_cyclos +> List.map i_to_s +> Common.join ", ")
(all_cyclos   +> List.map i_to_s +> Common.join ", ")
(List.length flaky_cyclos)
(List.length all_cyclos)
    in
    pr r_cmd;

    let tmp_file = Common.new_temp_file "stat" ".r" in
    Common.write_file ~file:tmp_file r_cmd;
    Sys.command(spf "R CMD BATCH %s" tmp_file) +> ignore;
  );
]
