(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* quite similar to the coverage heatmap *)
let properties = [
  "cyclo >100", "red3";
  "cyclo >50", "red1";
  "cyclo >20", "orange";
  "cyclo >10", "yellow";
  "cyclo >5", "YellowGreen";
  "cyclo >2", "green";
  "cyclo 2", "aquamarine3";
  "cyclo 1", "cyan";

  "noinfo", "white";
]

(* a little bit ugly *)
let property_of_cyclo n =
  match n with
  | _ when n > 100 -> "cyclo >100"
  | _ when n > 50 -> "cyclo >50"
  | _ when n > 20 -> "cyclo >20"
  | _ when n > 10 -> "cyclo >10"
  | _ when n > 5 -> "cyclo >5"
  | _ when n > 2 -> "cyclo >2"
  | 2 -> "cyclo 2"
  | 1 -> "cyclo 1"
  | 0 -> raise Impossible
  | _ -> raise Impossible

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer dir ~output =
  let dir = Common.realpath dir in

  (*  a layer needs readable paths, hence the www root computation *)

  let files = Lib_parsing_php.find_php_files_of_dir_or_files [dir] in

  let errors = ref [] in

  let layer = { Layer_code.
   title = "Cyclomatic complexity";
   description = "See http://en.wikipedia.org/wiki/Cyclomatic_complexity";
   files = files +> Common.index_list_and_total +> 
    List.map (fun (file, i, total) ->
      let ii_with_cyclo =
        try 
          pr2 (spf "processing: %s (%d/%d)" file i total);
          let ast = Parse_php.parse_program file in
        
          let (funcs, methods, topstatements) =
            Lib_parsing_php.functions_methods_or_topstms_of_program ast
          in
          (funcs +> List.map (fun def ->
            let iis = Lib_parsing_php.ii_of_any (Toplevel (FuncDef def)) in
            List.hd iis, Cyclomatic_php.cyclomatic_complexity_func def
          )) ++
          (methods +> List.map (fun def ->
            let iis = Lib_parsing_php.ii_of_any (ClassStmt (Method def)) in
            List.hd iis, Cyclomatic_php.cyclomatic_complexity_method def
          ))

        with exn ->
          Common.push2 (spf "PB with %s, exn = %s" file 
                           (Common.string_of_exn exn)) errors;
          []
      in
      let readable_file = Common.filename_without_leading_path dir file in
      readable_file,
      { Layer_code.
        micro_level = 
          ii_with_cyclo +> List.map (fun (ii, n) ->
            let line = Ast.line_of_info ii in
            line, property_of_cyclo n
          );
        macro_level = 
          match ii_with_cyclo with
          | [] -> ["noinfo", 1.]
          | xs -> 
              let max = xs +> List.map snd +> Common.maximum in
              [property_of_cyclo max, 1.]
        ;
      }
    );
    kinds = properties;
  }
  in
  Layer_code.save_layer layer output
             

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  "-gen_cyclomatic_layer", " <dir> <layerfile>",
  Common.mk_action_2_arg (fun dir output -> gen_layer dir ~output);
]
