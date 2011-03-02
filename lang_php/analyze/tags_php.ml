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
module V = Visitor_php
module Ast = Ast_php

module Tags = Tags_file

module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Making a better TAGS file. M-x idx => it finds it!
 * It does not go to $idx in a file. Work for XHP. Work with
 * completion.
 * 
 * Bench: time to process ~/www ? 7min the first time, which
 * is quite longer than ctags. But what is the price of correctness ?
 * Moreover one can easily put this into a cron and even shares
 * the results of such a cron to multiple developers via NFS.
 * 
 * Essentially a thin adapter of defs_uses_php.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tag_of_info filelines info =
  let line = Ast.line_of_info info in
  let pos = Ast.pos_of_info info in
  let col = Ast.col_of_info info in
  let s = Ast.str_of_info info in
  Tags.mk_tag (filelines.(line)) s line (pos - col)

let tag_of_name filelines name = 
  let info = Ast.info_of_name name in
  tag_of_info filelines info

(*****************************************************************************)
(* Main function *)
(*****************************************************************************)

let tags_of_ast ast filelines = 

  let ast = Unsugar_php.unsugar_self_parent_program ast in
  let defs = Defs_uses_php.defs_of_any (Program ast) in
    
    defs +> List.map (fun (kind, name, enclosing_name_opt) ->
      match kind with
      | Db.Function
      | Db.Class
      | Db.Interface 
      | Db.Constant
        ->
          [tag_of_name filelines name]
      | Db.Method ->
          (match enclosing_name_opt with
          | None -> raise Impossible
          | Some class_name ->
             (* Generate a 'class::method tag. Can then have
              * a nice completion and know all the methods available
              * in a class (the short Eiffel-like profile).
              * 
              * It used to also generate a 'method' tag with just
              * the method name, but if there is also a function
              * somewhere using the same name then this function
              * could be hard to reach.
              * 
              * alternative: could do first global analysis pass
              * to find all the functions and generate also the short
              * 'method' tag name when we are sure it would not
              * conflict with an existing function.
              *)
              let info = Ast.info_of_name name in
              let info' = Ast.rewrap_str 
                (Ast.name class_name  ^ "::" ^ Ast.name name) info in
              [tag_of_info filelines info';]
          )
      | ( Db.MultiDirs| Db.Dir| Db.File
        | Db.Field | Db.StaticMethod | Db.TopStmt | Db.Macro | Db.Global
        | Db.Type | Db.Module
        ) -> 
          []
    ) +> List.flatten

(* obsolete ? stuff with heavy_tagging ?
 * if heavy_tagging then begin
 * let info = Ast.info_of_name name in
 * let s = Ast.name name in
 * let info' = Ast.rewrap_str ("F_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 * let s = Ast.name name in
 * if heavy_tagging then begin
 * let info = Ast.info_of_name name in
 * let info' = Ast.rewrap_str ("C_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 * if heavy_tagging then begin
 * let info' = Ast.rewrap_str ("M_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let php_defs_of_files_or_dirs ?(verbose=false) xs =
  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> Common.index_list_and_total +> List.map (fun (file, i, total) ->
    if verbose then pr2 (spf "tagger: %s (%d/%d)" file i total);

    let (ast2, _stat) = Parse_php.parse_with_error_recovery file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let filelines = Common.cat_array file in
    let defs = 
      try tags_of_ast ast filelines 
      with 
      | Timeout -> raise Timeout
      | exn -> 
          pr2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
          []
    in
      
    (file, defs)
  )
  
