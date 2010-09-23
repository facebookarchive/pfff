
val php_defs_of_files_or_dirs:
  ?verbose:bool ->
  heavy_tagging:bool ->
  Common.path list ->
  (Common.filename * Tags_file.tag list) list
