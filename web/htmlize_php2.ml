(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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
open Eliom_pervasives
open Common

module Ast = Ast_php
module Db = Database_php
module HC = Highlight_code

module H = HTML5.M

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * A better htmlizer, using semantic information ! 
 *
 * Right now I use the xHTML.ml module from Thorsten Ohl.
 * The types can be quite tricky and XHTML.M above contain
 * nested modules which makes it hard to follow. I hate modules!
 * When pretty printing, especially when you want the pretty
 * printer to respect the newlines you've put in the html element, you
 * need to pass the ~preformatted label to the function.
 * 
 * todo? use xmlp4 ?
 * 
 * This is mostly copy paste of the one in lang_php/analyze/ but using
 * the XHTML of ocsigen. Also do not need to pretty print the
 * html as we return the typed html directly to the app.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: need typed CSS ! *)
let style = "
      body {
        color: #f5deb3;
        background-color: #2f4f4f;
      }
      .default {
        /* default */
        color: #f5deb3;
        background-color: #2f4f4f;
      }
      a { color:#ccf; }
"

let decorate_pcdata_with_attributes pcdata attrs =
  attrs |> List.fold_left (fun acc attr ->
    match attr with
    | `FOREGROUND s -> 
        let (r,g,b) = Simple_color.rgb_of_string s in
        let c = spf "#%02X%02X%02X" r g b in
        H.span ~a:[H.a_style (spf "color: %s" c)]
          [acc]
          
    | `SCALE scale ->
        (* [> `LARGE | `MEDIUM | `XX_LARGE | `X_LARGE ] *)
        acc
    | `STRIKETHROUGH abool ->
        acc
    | `STYLE style ->
        (*[> `ITALIC ] *)
        acc
    | `UNDERLINE style -> 
        (* [> `DOUBLE | `SINGLE ] *)
        acc
    | `WEIGHT bold ->
        (* [> `BOLD ] ] *)
        acc
    | `BACKGROUND s -> acc
        
  ) pcdata

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* Returns a list of Html elt, almost one for each token. When a token
 * is a comment we even generate one element per line.
 *)
let htmlize ~hook_token filename db =

  let ids = db.Db.file_to_topids#assoc filename in
  let asts_and_toks = ids +> List.map (fun id -> 
    id, 
    db.Db.defs.Db.toplevels#assoc id,
    db.Db.defs.Db.tokens_of_topid#assoc id
  ) 
  in
  let prefs = Highlight_code.default_highlighter_preferences in

  let inside_pre = 
    asts_and_toks |> List.map (fun (id, ast, toks) ->

      let h = Hashtbl.create 101 in

      let empty_hentities = Hashtbl.create 0 in

      Highlight_php.visit_toplevel 
        ~tag:(fun info categ -> Hashtbl.add h info categ)
        prefs  empty_hentities  (ast, toks);
      
      toks |> Common.map_filter (fun tok -> 
        let info = Token_helpers_php.info_of_tok tok in
        let s = Token_helpers_php.str_of_tok tok in

        if not (Ast.is_origintok info)
        then None
        else begin

        let categ = Common.hfind_option info h in
        let attrs =
          match categ with
          | None -> []
          | Some categ -> Highlight_code.info_of_category categ
        in

        (* old: decorate_pcdata_with_attributes (H.pcdata s) attrs *)

        let xs = Common.lines_with_nl_either s in
        let res = 
          xs +> List.map (function
          | Left s -> 
              (* by default would return H.pcdata s, but
               * could also be used to transform text into
               * clickable elements, as in lxr
               *)
              let data = hook_token s tok categ in
              decorate_pcdata_with_attributes data attrs
          | Right () ->
              (* no need for attributes for newlines *)
              H.pcdata "\n"
        )
        in
        Some res
        end
      ) |> List.flatten
    ) |> List.flatten
  in
  inside_pre

let htmlize_with_headers ~hook_token filename db = 
  let inside_pre = htmlize ~hook_token filename db in

  let html = 
    (H.html (*~a:[H.a_xmlns `W3_org_1999_xhtml; H.a_xml_lang "en"]*)
        (H.head
            (H.title (H.pcdata "XHTML")) [
            H.style [H.pcdata style ]
            ]
        )
        (H.body
            ([H.h1 [H.pcdata filename];
              H.pre (inside_pre)
            ])
        ))
  in
  html
