open Common

module J = Json_type
module H = Nethtml

let rec json_of_document doc = 
  match doc with
  | H.Element (name, args, subnodes) ->
      J.Array ([
        J.String "ELEMENT";
        J.String name; 
        J.Object (args +> List.map (fun (fld, value) ->
          fld, J.String value
        ))] ++
        (subnodes +> List.map json_of_document)
      )
  | H.Data s -> J.Array [J.String "DATA"; J.String s]

let rec json_of_html html = 
  J.Array (html +> List.map json_of_document)


let json_string_of_html html = 
  raise Todo
