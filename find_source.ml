open Common

let files_of_dir_or_files ~lang ~verbose xs =
  let xs = List.map Common.realpath xs in
  (match lang with
  | "php" | "phpfuzzy" -> Lib_parsing_php.find_php_files_of_dir_or_files xs
  | "c++" -> Lib_parsing_cpp.find_source_files_of_dir_or_files xs
  | "ml" -> Lib_parsing_ml.find_source_files_of_dir_or_files xs
  | "java" -> Lib_parsing_java.find_source_files_of_dir_or_files xs
  | "js"  -> Lib_parsing_js.find_source_files_of_dir_or_files xs
  | _ -> failwith ("unsupported language: " ^ lang)
  ) +> Skip_code.filter_files_if_skip_list ~verbose
