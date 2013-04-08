open Common (* for +> *)

open Ast_php_simple

let verbose = ref true

let is_valid (path : string) : bool =
  let autoload = Str.regexp ".*autoload_map.php" in
  not (Str.string_match autoload path 0)

let find_methods_return_this_version1 dir =
  let files = Lib_parsing_php.find_php_files_of_dir_or_files [dir] in
  files +> Common_extra.progress ~show:!verbose (fun progress_callback ->
    List.iter (fun file ->
      progress_callback();
      let cst = Parse_php.parse_program file in
      let ast = Ast_php_simple_build.program_with_position_information cst in
      ast +> List.iter (fun top ->
        match top with
        | ClassDef def ->
            List.iter (fun method_def ->
              match method_def.f_body with
              | [] -> ()
              | xs ->
                let last = Common2.list_last xs in
                (match last with
                | Return(Some(This(_))) ->
                    pr2 (spf "Found a match in %s %s"
                           file (Ast_php_simple.str_of_name method_def.f_name))
                | _ -> ()
                )
            ) def.c_methods
        | _ -> ()
      );
    )
  )

open Ast_php
(* the control flow graph currently works on Ast_php, not Ast_php_simple *)
module Ast = Ast_php
module CFG = Controlflow_php

let find_methods_return_this_version2 dir =
  let files = Lib_parsing_php.find_php_files_of_dir_or_files [dir] in
  let files = List.filter is_valid files in
  files +> List.iter (fun file ->
    let cst = Parse_php.parse_program file in
    let classes = cst +> Common.map_filter (function
      | ClassDef def -> Some def
      | _ -> None
    ) in
    classes +> List.iter (fun cdef ->
      cdef.c_body +> Ast.unbrace +> List.iter (fun class_stmt ->
        match class_stmt with
        | Method def ->
          let cfg = Controlflow_build_php.cfg_of_func def in
          let exit_nodei = CFG.find_exit cfg in
          let pred = cfg#predecessors exit_nodei in
          let has_only_return_this =
            pred#tolist +> List.for_all (fun (nodei, _edge) ->
              let node = cfg#nodes#assoc nodei in
              match node.CFG.n with
              | CFG.Return (Some(Lv(This _))) -> true
              | CFG.Return _ -> false
              (* when have empty else branch, implicit return void *)
              | CFG.FalseNode -> false
              | _ -> false

            )
          in
          Printf.printf
            "Match in %s %s %s %b\n"
                file
                (Ast_php.str_of_name cdef.c_name)
                (Ast_php.str_of_name def.f_name)
                (has_only_return_this)
        | _ -> ()
      )
    )
  )

let main =
  let func =
    match Sys.argv.(1) with
    | "1" -> find_methods_return_this_version1
    | "2" -> find_methods_return_this_version2
    | _ -> failwith "usage: find_methods_return_this 1|2 <phpfile or dir>"
  in
  func Sys.argv.(2)
