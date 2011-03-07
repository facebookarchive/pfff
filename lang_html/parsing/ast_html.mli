
type html_raw = HtmlRaw of string

type html_tree = 
  | Element of tag * (attr_name * attr_value) list * html_tree list
  | Data of string wrap

 and tag = Tag of string wrap
 and attr_name = Attr of string wrap
 and attr_value = Val of string wrap

  and info = Parse_info.info
  and 'a wrap = 'a * info

(* html type in ocamlnet *)
type html_tree2 = Nethtml.document list


val fakeInfo:
  ?next_to:(Parse_info.parse_info * int) option -> 
  ?str:string -> unit -> 
  Parse_info.info
