(*s: show_function_calls2.ml *)

(*s: basic pfff modules open *)
open Common
open Ast_php
(*e: basic pfff modules open *)
module V = Visitor_php

(*s: show_function_calls v2 *)
let show_function_calls file = 
  let (asts2, _stat) = Parse_php.parse file in
  let asts = Parse_php.program_of_program2 asts2 in

  (*s: create visitor *)
    let visitor = V.mk_visitor 
     { V.default_visitor with
       V.klvalue = (fun (k, _) var ->

        match Ast_php.untype var with
        | FunCallSimple (qu_opt, funcname, args) ->
            (*s: print funcname *)
            let s = Ast_php.name funcname in
            let info = Ast_php.info_of_name funcname in
            let line = Ast_php.line_of_info info in
            pr2 (spf "Call to %s at line %d" s line);
            (*e: print funcname *)

        | _ -> 
            (*s: visitor recurse using k *)
               k var
            (*e: visitor recurse using k *)
      );
    }
    in
  (*e: create visitor *)
  (*s: iter on asts using visitor *)
    asts |> List.iter visitor.V.vtop
  (*e: iter on asts using visitor *)
(*e: show_function_calls v2 *)

let main = 
  show_function_calls Sys.argv.(1)
(*e: show_function_calls2.ml *)
