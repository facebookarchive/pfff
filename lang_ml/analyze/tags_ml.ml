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
module Tags = Tags_file
module Db = Database_code
open Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Alternatives:
 *  - otags, but does not work very well recursively as it groks easily
 *    on unparable files.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
let tag_of_name filelines name = 
  let info = Ast.info_of_name name in
  tag_of_info filelines info
*)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let defs_of_files_or_dirs ?(verbose=false) xs =
  let files = Lib_parsing_ml.find_ml_files_of_dir_or_files xs in

  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.map (fun file ->
    k();
     let (ast2) = 
       try 
         Common.save_excursion Flag_parsing_ml.show_parsing_error false(fun()->
           Parse_ml.parse file +> fst
         )
       with Parse_ml.Parse_error pos ->
         pr2 (spf "PARSING error in %s" (Parse_info.string_of_info pos));
         []
     in
    let filelines = Common.cat_array file in
    let defs = ref [] in
    let h = Hashtbl.create 101 in

    ast2 +> List.iter (fun (ast, (_str, toks)) ->
      (* computing the token attributes *)
      let prefs = Highlight_code.default_highlighter_preferences in

      Highlight_ml.visit_toplevel 
        ~lexer_based_tagger:true (* !! *)
        ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
        prefs
        (ast, toks)
      ;

      (* processing the tokens in order *)
      toks +> List.iter (fun tok -> 

        let info = Token_helpers_ml.info_of_tok tok in
        let s = Token_helpers_ml.str_of_tok tok in

        let categ = Common.hfind_option info h in
        
        categ +> Common.do_option (fun x ->
          match x with
          | Function (Def2 _)
          | Global (Def2 _)
          | Module Def
          | TypeDef Def

          | FunctionDecl _
            -> 
              let kind =
                match x with
                | Function (Def2 _) -> Db.Function
                | Global (Def2 _) -> Db.Global
                | Module Def -> Db.Module
                | TypeDef Def -> Db.Type
                | FunctionDecl _ -> Db.Function
                | _ -> raise Impossible
              in

              Common.push2 (Tags.tag_of_info filelines info kind) defs;

              let (d,b,e) = Common.dbe_of_filename file in
              let module_name = String.capitalize b in

              let info' = Parse_info.rewrap_str (module_name ^ "." ^ s) info in

              (* I prefer my tags to led me to the .ml file rather than
               * the .mli because the .mli is usually small and
               * I have a key to go from the .ml to .mli anyway.
               *)

              if e = "ml" ||
                 (e = "mli" && not (Sys.file_exists
                                      (Common.filename_of_dbe (d,b, "ml"))))
              then
                Common.push2 (Tags.tag_of_info filelines info' kind) defs;
          | _ -> ()
        )
      );
    );

(*
    let visitor = V.mk_visitor { V.default_visitor with
      V.kfunc_def = (fun (k, _) def ->
        let name = def.f_name in
        let info = Ast.info_of_name name in
        Common.push2 (tag_of_name filelines name) defs;
        let s = Ast.name name in

        if heavy_tagging then begin
          let info' = Ast.rewrap_str ("F_" ^ s) info in
          Common.push2 (tag_of_info filelines info') defs;
        end;
        
        k def
      );

      V.kclass_def = (fun (k, _) def ->
        let name = def.c_name in
        let info = Ast.info_of_name name in
        let s = Ast.name name in
        Common.push2 (tag_of_name filelines name) defs;
        
        if heavy_tagging then begin
          let info' = Ast.rewrap_str ("C_" ^ s) info in
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
    }
    in
    visitor.V.vprogram ast;
*)
      
    let defs = List.rev (!defs) in
    (file, defs)
  ))
  
