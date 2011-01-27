(*s: parsing2.mli *)

val tokens_with_categ_of_file :
  Common.filename ->
  (string, Database_code.entity) Hashtbl.t ->
  (string * Highlight_code.category option * Common.filepos) list

val use_arity_of_use_count : int -> Highlight_code.use_arity

type ast =
  | ML of Parse_ml.program2
  | Hs  of Parse_hs.program2

  | Php of Parse_php.program2
  | Js of Parse_js.program2

  | Cpp of Parse_cpp.program2

  | Csharp of Parse_csharp.program2
  | Java of Parse_java.program2

  | Lisp of Parse_lisp.program2
  | Erlang of Parse_erlang.program2

  | Python of Parse_python.program2

  | Noweb of Parse_nw.program2

val _hmemo_file : (Common.filename, ast) Hashtbl.t
val disable_file_in_cache : 
  Common.filename -> unit
(*e: parsing2.mli *)
