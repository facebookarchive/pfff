open Common

module J = Json_type
module H = Ast_html

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
  | H.Element (name, args, subnodes) ->
      J.Array ([
        J.String "ELEMENT";
        J.String name; 
        J.Object (args +> List.map (fun (fld, value) ->
          fld, J.String value
        ))] ++
        (subnodes +> List.map json_of_html_tree)
      )
  | H.Data s -> J.Array [J.String "DATA"; J.String s]

let json_string_of_html_tree html = 
  raise Todo
