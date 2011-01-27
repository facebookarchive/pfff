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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Making a better TAGS file. M-x idx => it finds it!
 * It does not go to $idx in a file. Work for XHP. Work with
 * completion.
 * 
 * Bench: time to process ~/www ? 7min the first time, which
 * is quite longer than ctags. But what is the price of correctness ?
 * Moreover one can easily put this into a cron and even shares
 * the results of such a cron to multiple developers via NFS.
 * 
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

(* todo: use defs_php.ml instead *)
let tags_of_ast ~heavy_tagging ast filelines = 

    let defs = ref [] in
    let current_class = ref "" in

    let visitor = V.mk_visitor { V.default_visitor with
      V.kfunc_def = (fun (k, _) def ->
        let name = def.f_name in
        Common.push2 (tag_of_name filelines name) defs;

        if heavy_tagging then begin
          let info = Ast.info_of_name name in
          let s = Ast.name name in
          let info' = Ast.rewrap_str ("F_" ^ s) info in
          Common.push2 (tag_of_info filelines info') defs;
        end;
        
        k def
      );

      V.kclass_def = (fun (k, _) def ->
        let name = def.c_name in
        Common.push2 (tag_of_name filelines name) defs;
        
        let s = Ast.name name in
        if heavy_tagging then begin
          let info = Ast.info_of_name name in
          let info' = Ast.rewrap_str ("C_" ^ s) info in
          Common.push2 (tag_of_info filelines info') defs;
        end;
        
        Common.save_excursion current_class s (fun () ->
          k def;
        );
      );

      V.kinterface_def = (fun (k, _) def ->
        let name = def.i_name in
        Common.push2 (tag_of_name filelines name) defs;
        
        let s = Ast.name name in
        if heavy_tagging then begin
          let info = Ast.info_of_name name in
          let info' = Ast.rewrap_str ("I_" ^ s) info in
          Common.push2 (tag_of_info filelines info') defs;
        end;
        
        Common.save_excursion current_class s (fun () ->
          k def;
        );
      );

      V.kmethod_def = (fun (k, _) def ->
        let name = def.m_name in
        let info = Ast.info_of_name name in
        
        Common.push2 (tag_of_name filelines name) defs;
        (* also generate a A::xxx tag to help completion *)
        let s = Ast.str_of_info info in
        let info' = Ast.rewrap_str (!current_class ^ "::" ^ s) info in
        Common.push2 (tag_of_info filelines info') defs;
        
        if heavy_tagging then begin
          let info' = Ast.rewrap_str ("M_" ^ s) info in
          Common.push2 (tag_of_info filelines info') defs;
        end;
      );

      V.klvalue = (fun (k, bigf) x ->
        match Ast.untype x with

        | FunCallSimple((Name ("define", tok)), args) ->
            let args = args |> Ast.unparen |> Ast.uncomma in
            (match args with
            (* TODO? maybe better to have a Define directly in the AST ? 
             * is it specific to facebook ? 
             *)
            | (Arg ((Sc (C (String (s,info)))), _t))::xs -> 
                (* by default the info contains the '' or "" around the string,
                 * which is not the case for s. See ast_php.ml
                 *)
                let info' = Ast.rewrap_str (s) info in
                Common.push2 (tag_of_info filelines info') defs;
            | _ -> ()
            )
        | _ -> k x
      );
    }
    in
    visitor (Program ast);
    let defs = List.rev (!defs) in
    defs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let php_defs_of_files_or_dirs ?(verbose=false) ~heavy_tagging xs =
  let files = Lib_parsing_php.find_php_files_of_dir_or_files xs in

  files +> Common.index_list_and_total +> List.map (fun (file, i, total) ->
    if verbose then pr2 (spf "tagger: %s (%d/%d)" file i total);

    let (ast2, _stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    Lib_parsing_php.print_warning_if_not_correctly_parsed ast file;

    let filelines = Common.cat_array file in

    let defs = tags_of_ast ~heavy_tagging ast filelines in
      
    (file, defs)
  )
  
