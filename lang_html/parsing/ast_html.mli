
type info = Parse_info.info

type html_raw = HtmlRaw of string

type html_tree = 
  | Element of tag * (attr_name * attr_value) list * html_tree list
  | Data of string
 and tag = string
 and attr_name = string
 and attr_value = string

(* html type in ocamlnet *)
type html_tree2 = Nethtml.document list
