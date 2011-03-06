
type info = Parse_info.info

type html_raw = HtmlRaw of string

type html_tree = 
  | Element of tag * (attr_name * attr_value) list * html_tree list
  | Data of string
 and tag = Tag of string
 and attr_name = Attr of string
 and attr_value = Val of string

(* html type in ocamlnet *)
type html_tree2 = Nethtml.document list
