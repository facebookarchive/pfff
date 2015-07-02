
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* OCaml piqi types are converted to xml/json/piqi/pb strings,and wrote to file.
 * Separate functions writtent here because: Piqirun_ext.output_format is 
 * resolved here but not elsewhere. Needs more inspection.
 *
 * For writing graph_to_graphson to other formats (xml/pb/piqi) pls replace
 * `json with equivalent. Is hardcoded json because, GraphSON is by definition
 * json. If you want to write to other formats and graph_to_piqi is too slow,
 * this can be done.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_output_format = function 
    | "piq" -> `piq
    | "pb" -> `pb
    | "json" -> `json_pretty
    | "xml" -> `xml
    | "json_pretty" -> `json_pretty
    | "xml_pretty" -> `xml_pretty
    | "pib" -> `pib
    | _ -> `pb

(*****************************************************************************)
(* Graph_to_graphson *)
(*****************************************************************************)

let graphson_options g output_format_string =
  let output_format = string_output_format output_format_string in
  Graph_graphson_piqi_ext.gen_graph_graphson g output_format

(* Appends vertex to file Graphson.json *)
let vertex_graphson vertex =
  let v = Graph_graphson_piqi_ext.gen_vertex vertex `json in
  Common.append_file "GraphSON.json" ("," ^ v) 

(* Appends edge to file Edges.json *)
let edge_graphson edge =
  let e = Graph_graphson_piqi_ext.gen_edge_graphson edge `json in
  Common.append_file "Edges.json" ("," ^ e)

(* Creates/overwrites file GraphSON.json and writes initial vertex *)
let create_initial_vertex vertex =
  let initial_str = "{ \"mode\" : \"NORMAL\", \n \"vertices\" : [ \n" in
  let v = Graph_graphson_piqi_ext.gen_vertex vertex `json in
  Common.write_file "GraphSON.json" (initial_str ^ v ^ "\n")

(* Creates/overwrites file Edges.json and writes inital edge *)
let create_initial_edge edge =
  let initial_str = "\"edges\" : [ \n" in
  let e = Graph_graphson_piqi_ext.gen_edge_graphson edge `json in
  Common.write_file "Edges.json" (initial_str ^ e ^ "\n")

(* Finishes up format for vertices and eges in Edges.json and GraphSON.json and
 * combines them to GraphSON.json.
 *
 * FIXME: cleanup_graphson is called in the beginning, even though call is
 * written after all edge_graphson and vertex_graphson operations are made.
 * Something to do with the way threads are handled in OCaml I think. Shell
 * script does what this function is supposed to do currently.
 *)
let merge_graphson () =
  Common.append_file "GraphSON.json" ("], \n");
  Common.append_file "Edges.json" ("] } \n");
  let str = Common.read_file "Edges.json" in
  Common.append_file "GraphSON.json" str
