let version = "0.12"

let path = 
  try (Sys.getenv "PFFF_HOME") 
  with Not_found->"/home/pad/orig-pfff"

let std_xxx = ref (Filename.concat path "xxx.yyy")
