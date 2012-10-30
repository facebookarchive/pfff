val defs_of_dir_or_file:
  ?verbose:bool ->
  Common.path -> Skip_code.skip list ->
  (Common.filename * Tags_file.tag list) list
