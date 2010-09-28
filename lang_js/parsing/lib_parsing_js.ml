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

open Ast_js

module Ast = Ast_js
module Flag = Flag_parsing_js

module V = Visitor_js

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

(* copy paste of php code, not sure relevant for javascript *)
let is_js_script file = 
  Common.with_open_infile file (fun chan ->
    try 
      let l = input_line chan in
      l =~ "#!/usr/.*/js" ||
      l =~ "#!/bin/env js" ||
      l =~ "#!/usr/bin/env js"

    with End_of_file -> false
  )

let find_js_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    (* could use let ftype = File_type.file_type_of_file filename in *)

    (filename =~ ".*\\.js$") ||
      (filename =~ ".*\\.javascript$") ||
      is_js_script filename
  ) |> Common.sort

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

let extract_info_visitor recursor = 
  let globals = ref [] in
  let hooks = { V.default_visitor with
    V.kinfo = (fun (k, _) i -> Common.push2 i globals)
  } in
  begin
    let vout = V.mk_visitor hooks in
    recursor vout;
    List.rev !globals
  end

let ii_of_toplevel top = 
  extract_info_visitor (fun visitor -> visitor.V.vtop top)
let ii_of_expr e = 
  extract_info_visitor (fun visitor -> visitor.V.vexpr e)

(*****************************************************************************)
(* Max min, range *)
(*****************************************************************************)
let min_max_ii_by_pos xs = 
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs -> 
      let pos_leq p1 p2 = (Parse_info.compare_pos p1 p2) =|= (-1) in
      xs +> List.fold_left (fun (minii,maxii) e -> 
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        minii', maxii'
      ) (x,x)
