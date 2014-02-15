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

open Common

open Ast_web

let ml_pattern_string_of_web_document webdoc = 
  let s_html = Export_html.ml_pattern_string_of_html_tree webdoc.html in
  let s_css =  
    webdoc.css +> List.map (fun (_s, css) ->
      Export_ast_css.ml_pattern_string_of_stylesheet css
    ) +> Common2.unlines
  in
  let s_js =
    webdoc.js +> List.map (fun (_s, js) ->
      let v = Meta_ast_js.vof_program js in
      Ocaml.string_of_v v
    ) +> Common2.unlines
  in
  s_html ^ "\n" ^ s_css ^ "\n" ^ s_js

