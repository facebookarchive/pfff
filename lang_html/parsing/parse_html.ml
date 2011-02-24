
open Ast_html 


(* a small wrapper over ocamlnet *)
let (parse: html_raw -> html) = fun raw -> 
  let ch = new Netchannels.input_string raw in
  Nethtml.parse ch
