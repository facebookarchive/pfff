open Common


let finder lang = 
  match lang with
  | "php" | "phpfuzzy" -> 
    Lib_parsing_php.find_php_files_of_dir_or_files ~verbose:false
  | "c++" -> 
    Lib_parsing_cpp.find_source_files_of_dir_or_files
  | "ml" -> 
    Lib_parsing_ml.find_source_files_of_dir_or_files
  | "java" -> 
    Lib_parsing_java.find_source_files_of_dir_or_files
  | "js"  -> 
    Lib_parsing_js.find_source_files_of_dir_or_files ~include_scripts:false
  | _ -> failwith ("unsupported language: " ^ lang)

let files_of_dir_or_files ~lang ~verbose xs =
  let xs = List.map Common.realpath xs in
  let finder = finder lang in
  finder xs +> Skip_code.filter_files_if_skip_list ~verbose
