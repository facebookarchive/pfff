(*s: parsing2.ml *)
(*s: Facebook copyright *)
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
(*e: Facebook copyright *)

open Common

module FT = File_type

module HC = Highlight_code
module Db = Database_code

open Highlight_code

(*****************************************************************************)
(* Parsing helpers *)
(*****************************************************************************)

(* This type is needed if we want to use a single hashtbl to memoize
 * all the parsed file. Having a single hash helps for 
 * disable_file_in_cache below.
 *)
type ast = 
  | ML  of Parse_ml.program2
  | Php of Parse_php.program2
  | Cpp of Parse_cpp.program2
  | Js  of Parse_js.program2

  | Noweb of Parse_nw.program2


let _hmemo_file = Hashtbl.create 101

let parse_ml2 file = 
  Common.memoized _hmemo_file file (fun () -> 
    ML (Parse_ml.parse file +> fst))
let parse_ml_cache a = 
  Common.profile_code "View.parse_ml_cache" (fun () -> 
    match parse_ml2 a with | ML a -> a | _ -> raise Impossible
  )

let parse_nw2 file = 
  Common.memoized _hmemo_file file (fun () -> 
    Noweb (Parse_nw.parse file +> fst))
let parse_nw_cache a = 
  Common.profile_code "View.parse_nw_cache" (fun () -> 
    match parse_nw2 a with | Noweb a -> a | _ -> raise Impossible
  )


let parse_php2 file = 
  Common.memoized _hmemo_file file (fun () -> 
    let (ast2, stat) = Parse_php.parse file in
    let ast = Parse_php.program_of_program2 ast2 in
    (* work by side effect on ast2 too *)
    Check_variables_php.check_and_annotate_program
      ast;
    Php ast2
  )
let parse_php_cache a = 
  Common.profile_code "View.parse_php_cache" (fun () -> 
    match parse_php2 a with | Php a -> a | _ -> raise Impossible
  )

let parse_js2 file = 
  Common.memoized _hmemo_file file (fun () -> 
    Js (Parse_js.parse file +> fst))
let parse_js_cache a = 
  Common.profile_code "View.parse_js_cache" (fun () -> 
    match parse_js2 a with | Js a -> a | _ -> raise Impossible
  )

let parse_cpp2 file = 
  Common.memoized _hmemo_file file (fun () -> 
    Cpp (Parse_cpp.parse file +> fst))
let parse_cpp_cache a = 
  Common.profile_code "View.parse_cpp_cache" (fun () -> 
    match parse_cpp2 a with | Cpp a -> a | _ -> raise Impossible
  )




let disable_file_in_cache file =
  Hashtbl.remove _hmemo_file file


(*****************************************************************************)
(* Semantic ehancement *)
(*****************************************************************************)

let use_arity_of_use_count n =
  match () with
  (* note that because my PHP object analysis have some threshold
   * on the number of callers (see threshold_callers_indirect_db)
   * the number for HugeUse can not be more than this one otherwise
   * you will miss some cases
   *)
  | _ when n >= 100 -> HugeUse

  | _ when n > 20 -> LotsOfUse
  | _ when n >= 10  -> MultiUse
  | _ when n >= 2 -> SomeUse
  | _ when n = 1 -> UniqueUse
  | _ -> NoUse

let rewrite_categ_using_entities s categ file entities =

  let e_kind_opt = 
    try Some (Db.entity_kind_of_highlight_category categ)
    with _ -> None
  in
  match e_kind_opt with
  | None -> categ
  | Some e_kind ->

  
   let entities = 
    Hashtbl.find_all entities s +> List.filter (fun e ->
      (* we could have the full www dbcode but run the treemap on
       * a subdir in which case the root will not be the same.
       * It's a good approximation to just look at the basename.
       * The only false positive we will get if another file,
       * with the same name happened to also define entities
       * with the same name, which would be rare.
       * 
       * update: TODO use Model2.readable_to_absolute_filename_under_root ?
       *)
      Filename.basename e.Db.e_file = Filename.basename file &&
      (* some file have both a function and class with the same name *)
      e.Db.e_kind = e_kind
    )
  in
  match entities with
  | [] -> categ
  | [e] ->
      let use_cnt = e.Db.e_number_external_users in
      let arity = use_arity_of_use_count use_cnt in
      (* coupling: you must also say in style2 to take into account
       * the arity for the category
       *)
      (match categ with
      | Function (Def2 _) ->
          Function (Def2 arity)
      | FunctionDecl _ ->
          FunctionDecl (arity)
      | Class (Def2 _) ->
          Class (Def2 arity)
      | Method (Def2 _) ->
          Method (Def2 arity)
      | StaticMethod (Def2 _) ->
          StaticMethod (Def2 arity)
      | _ -> categ
      )
  | x::y::xs ->
      (* TODO: handle __construct directly *)
      if not (List.mem s ["__construct"])
      then
        pr2_once (spf "multi def found for %s in %s" s file);
      categ

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* coupling: right now if you add a language here, you need to whitelist it
 * also in draw2.draw_contents2.
 * 
 * todo: ugly, lots of repetitive code. If factorize code in
 * parse_info.ml can at least factorize some of the Ast_xxx.str_of_xxx.
 *)
let tokens_with_categ_of_file file hentities = 
  let ftype = FT.file_type_of_file file in
  let prefs = Highlight_code.default_highlighter_preferences in


  match ftype with
  | FT.PL (FT.Web (FT.Php _)) ->
      let h = Hashtbl.create 101 in

      let ast2 = parse_php_cache file in
      ast2 +> List.map (fun (ast, (_str, toks)) ->
        (* computing the token attributes *)
        Highlight_php.visit_toplevel 
          ~tag:(fun info categ -> Hashtbl.add h info categ)
          ~maybe_add_has_type_icon:(fun a b c -> ())
          prefs
          None
          (ast, toks)
        ;
      
        (* getting the text *)
        toks |> Common.map_filter (fun tok -> 
          let info = Token_helpers_php.info_of_tok tok in
          let s = Token_helpers_php.str_of_tok tok in

          if not (Ast_php.is_origintok info)
          then None
          else 
            let categ = Common.hfind_option info h in
            let categ = categ +> Common.fmap (fun categ ->
                rewrite_categ_using_entities s categ file hentities
              )
            in
            Some (s, categ, 
                 { l = Ast_php.line_of_info info;
                   c = Ast_php.col_of_info info;
                 })
        )
      ) +> List.flatten

  | FT.PL (FT.ML _) ->
      let h = Hashtbl.create 101 in

      let ast2 = parse_ml_cache file in
      ast2 +> List.map (fun (ast, (_str, toks)) ->
        (* computing the token attributes *)
        Highlight_ml.visit_toplevel 
          ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
          prefs
          (ast, toks)
        ;

        (* getting the text *)
        toks |> Common.map_filter (fun tok -> 
          let info = Token_helpers_ml.info_of_tok tok in
          let s = Token_helpers_ml.str_of_tok tok in

          if not (Parse_info.is_origintok info)
          then None
          else 
            let categ = Common.hfind_option info h in
            let categ = categ +> Common.fmap (fun categ ->
                rewrite_categ_using_entities s categ file hentities
              )
            in
            Some (s, categ,
                 { l = Ast_ml.line_of_info info;
                   c = Ast_ml.col_of_info info;
                 })

        )
      ) +> List.flatten

  | FT.Text ("nw" | "tex" | "texi" | "web") ->

      let h = Hashtbl.create 101 in

      let ast2 = parse_nw_cache file in
      ast2 +> List.map (fun (ast, (_str, toks)) ->
        (* computing the token attributes *)
        Highlight_nw.visit_toplevel 
          ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
          prefs
          (ast, toks)
        ;

        (* getting the text *)
        toks |> Common.map_filter (fun tok -> 
          let info = Token_helpers_nw.info_of_tok tok in
          let s = Token_helpers_nw.str_of_tok tok in

          if not (Parse_info.is_origintok info)
          then None
          else 
            let categ = Common.hfind_option info h in
            let categ = categ +> Common.fmap (fun categ ->
                rewrite_categ_using_entities s categ file hentities
              )
            in
            Some (s, categ,
                 { l = Parse_info.line_of_info info;
                   c = Parse_info.col_of_info info;
                 })

        )
      ) +> List.flatten


  | FT.PL (FT.Cplusplus _ | FT.C | FT.Thrift) ->
      let h = Hashtbl.create 101 in

      let ast2 = parse_cpp_cache file in
      ast2 +> List.map (fun (ast, (_str, toks)) ->
        (* computing the token attributes *)
        Highlight_cpp.visit_toplevel 
          ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
          prefs
          (ast, toks)
        ;

        (* getting the text *)
        toks |> Common.map_filter (fun tok -> 
          let info = Token_helpers_cpp.info_of_tok tok in
          let s = Token_helpers_cpp.str_of_tok tok in

          if not (Ast_cpp.is_origintok info)
          then None
          else 
            let categ = Common.hfind_option info h in
            let categ = categ +> Common.fmap (fun categ ->
                rewrite_categ_using_entities s categ file hentities
              )
            in
            Some (s, categ,
                 { l = Ast_cpp.line_of_info info;
                   c = Ast_cpp.col_of_info info;
                 })

        )
      ) +> List.flatten

  | FT.PL (FT.Web (FT.Js _)) ->

      let h = Hashtbl.create 101 in

      let ast2 = parse_js_cache file in
      ast2 +> List.map (fun (ast, (_str, toks)) ->
        (* computing the token attributes *)
        Highlight_js.visit_toplevel 
          ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
          prefs
          (ast, toks)
        ;

        (* getting the text *)
        toks |> Common.map_filter (fun tok -> 
          let info = Token_helpers_js.info_of_tok tok in
          let s = Token_helpers_js.str_of_tok tok in
          let s = Ast_js.remove_quotes_if_present s in

          if not (Ast_js.is_origintok info)
          then None
          else 
            let categ = Common.hfind_option info h in
            let categ = categ +> Common.fmap (fun categ ->
                rewrite_categ_using_entities s categ file hentities
              )
            in
            Some (s, categ,
                 { l = Ast_js.line_of_info info;
                   c = Ast_js.col_of_info info;
                 })

        )
      ) +> List.flatten



  | _ -> failwith 
      "impossible: should be called only when file has good file_kind"
(*e: parsing2.ml *)
