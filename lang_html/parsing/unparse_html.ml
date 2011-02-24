
open Ast_html

let (unparse: html -> html_raw) = fun html -> 
  let buf = Buffer.create 1000 in
  let ch = new  Netchannels.output_buffer buf in
  Nethtml.write ch html;
  Buffer.contents buf
