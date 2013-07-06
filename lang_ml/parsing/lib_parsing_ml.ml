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

open Ast_ml

module Ast = Ast_ml
module Flag = Flag_parsing_ml
module V = Visitor_ml

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

let find_ml_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  +> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | File_type.PL (File_type.ML ("ml" | "mli")) -> 
        (* a little bit pad specfic, syncweb metadata *)
        not (filename =~ ".*\\.md5sum")
    | _ -> false
  ) +> Common.sort

let find_cmt_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
   +> List.filter (fun filename->
    match File_type.file_type_of_file filename with
    | File_type.Obj "cmt" -> true
    | _ -> false
  ) +> Common.sort

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

let ii_of_any any = 
  extract_info_visitor (fun visitor -> visitor any)

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)
let is_function_body x = 
  match Ast.uncomma x with
  | (Fun _ | Function _)::xs -> true
  | _ -> false

