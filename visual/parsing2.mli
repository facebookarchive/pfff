(*s: parsing2.mli *)

val tokens_with_categ_of_file :
  Common.filename ->
  (string, Database_code.entity) Hashtbl.t ->
  (string * Highlight_code.category option * Common.filepos) list

val use_arity_of_use_count : int -> Highlight_code.use_arity

type ast =
    Php of Parse_php.program2
  | ML of Parse_ml.program2
  | Cpp of Parse_cpp.program2
  | Js of Parse_js.program2
val _hmemo_file : (Common.filename, ast) Hashtbl.t
val disable_file_in_cache : 
  Common.filename -> unit
(*e: parsing2.mli *)
