
val _errors : string Common.stack ref
val pr2 : string -> unit
val pr2_once : string -> unit
val pr2_err : string -> unit

val users_of_class_in_any : Ast_php.any -> Ast_php.name list
exception NoII
val first_filepos_origin : Ast_php.info list -> Entity_php.filepos
val fpos_of_toplevel : Ast_php.toplevel -> Entity_php.filepos
val fpos_of_idast : Ast_php.entity -> Entity_php.filepos
val prange_of_origin_tokens :
  Parser_php.token list -> Parse_info.parse_info * Parse_info.parse_info
val first_comment :
  Ast_php.toplevel -> Parser_php.token list -> Parse_info.parse_info option
val get_newid : Database_php.database -> Entity_php.id
val add_toplevel2 :
  string ->
  Ast_php.toplevel * (string * Parser_php.token list) ->
  Database_php.database -> Entity_php.id
val add_filename_and_topids :
  Common.filename * Database_php.id list -> Database_php.database -> unit
val add_nested_id_and_ast :
  enclosing_id:Database_php.id ->
  Ast_php.entity -> Database_php.database -> Database_php.id
val add_def :
  Database_php.id_string * Database_php.id_kind * Database_php.id *
  Ast_php.name option -> Database_php.database -> unit
val add_callees_of_id2 :
  Database_php.id * Namespace_php.nameS Ast_php.wrap list -> 
  Database_php.database -> unit
val add_callees_of_id :
  Database_php.id * Namespace_php.nameS Ast_php.wrap list -> 
  Database_php.database -> unit
val add_methodcallees_of_id : 'a * 'b -> 'c -> 'd


(* helpers used internally that can be useful to other *)
val iter_files: 
  Database_php.database -> 
  ((Common.filename * Database_php.id list) * 
  int (* idx *) * int (* total *) -> unit) ->
  unit

val iter_files_and_topids :
  Database_php.database -> string -> 
  (Database_php.id -> Common.filename -> unit) -> 
  unit

val iter_files_and_ids :
  Database_php.database -> string -> 
  (Database_php.id -> Common.filename -> unit) -> 
  unit
