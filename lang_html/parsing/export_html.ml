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

open Ast_html
module H = Ast_html

module J = Json_type
module H2 = Nethtml


(* obsolete *)
let rec json_of_document doc = 
  match doc with
  | H2.Element (name, args, subnodes) ->
      J.Array ([
        J.String "ELEMENT";
        J.String name; 
        J.Object (args +> List.map (fun (fld, value) ->
          fld, J.String value
        ))] ++
        (subnodes +> List.map json_of_document)
      )
  | H2.Data s -> J.Array [J.String "DATA"; J.String s]
let rec json_of_html_tree2 html = 
  J.Array (html +> List.map json_of_document)


let rec json_of_html_tree doc = 
  match doc with
  | H.Element (Tag (name, _tok), args, subnodes) ->
      J.Array ([
        J.String "ELEMENT";
        J.String name; 
        J.Object (args +> List.map (fun (Attr (fld, _t1), Val (value, _t2)) ->
          fld, J.String value
        ))] ++
        (subnodes +> List.map json_of_html_tree)
      )
  | H.Data (s, _tok) -> J.Array [J.String "DATA"; J.String s]

let json_string_of_html_tree html = 
  raise Todo
