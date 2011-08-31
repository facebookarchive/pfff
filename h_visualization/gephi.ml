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

module G = Graph

open Xml_types

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * Wrappers to use Gephi (http://gephi.org/), and to generate data
 * in its GEFX format.
 *)


(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* see http://gexf.net/format/ *)
let graph_to_gefx ~str_of_node ~output g =
  Common.with_open_outfile output (fun (pr_no_nl, _chan) ->
    let nodes = G.nodes g in

    let nodes_xml = nodes +> List.map (fun n ->
      Element ("node", [
        "id", i_to_s (G.ivertex n g);
        "label", str_of_node n;
      ], [])
    )
    in
    let edges_xml = nodes +> List.map (fun n ->
      let succ = G.succ n g in
      succ +> List.map (fun n2 ->
        Element ("edge", [
          "source", i_to_s (G.ivertex n g);
          "target", i_to_s (G.ivertex n2 g);
        ], [])
    )) +> List.flatten
    in
    let xml =
      Element (
        "gexf", [
          "xmlns", "http://www.gexf.net/1.2draft";
          "version", "1.2";
        ], [
          Element (
            "meta", [
              "lastmodifieddate", "2011-03-20";
            ], [
              Element ("creator", [], [PCData "pfff"]);
              Element ("description", [], [PCData "yep"]);
            ]);
          Element (
            "graph", [
              "mode", "static";
              "defaultedgetype", "directed";
            ], [
              Element ("nodes", [], nodes_xml);
              Element ("edges", [], edges_xml);
            ]
          )
        ]
      )
    in
    let s = Xml_parse.to_string_fmt xml in
    pr_no_nl s
  )
