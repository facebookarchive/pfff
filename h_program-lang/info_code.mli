
type info_txt = 
  (string * string list) list

type info_txts = unit
  
val load: Common.filename -> info_txt
