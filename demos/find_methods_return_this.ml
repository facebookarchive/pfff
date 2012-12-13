open Common (* for +> *)

open Ast_php_simple

let verbose = ref true

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
                let last = Common.list_last xs in
                (match last with
                | Return(_, Some(This(_))) ->
                    pr2 (spf "Found a match in %s %s" 
                           file (Ast_php_simple.str_of_name method_def.f_name))
                | _ -> ()
                )
            ) def.c_methods
        | _ -> ()
      );
    )
  )

let main = find_methods_return_this_version1 Sys.argv.(1)
