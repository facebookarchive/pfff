let version = "0.21"

let path = 
  try (Sys.getenv "PFFF_HOME") 
  with Not_found->"/usr/local/share/pfff"

let std_xxx = ref (Filename.concat path "xxx.yyy")

let logger = 
  try Some (Sys.getenv "PFFF_LOGGER")
  with Not_found-> None
