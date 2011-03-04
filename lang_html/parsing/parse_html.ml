
open Ast_html 


(* a small wrapper over ocamlnet *)
let (parse_simple_tree: html_raw -> html_tree2) = fun (HtmlRaw raw) -> 
  let ch = new Netchannels.input_string raw in
  Nethtml.parse ch
