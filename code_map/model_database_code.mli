(*s: model_database_code.mli *)

(*s: hentities sig *)
val hentities :
  Common.path -> Database_code.database option -> 
  (string, Database_code.entity) Hashtbl.t
(*e: hentities sig *)

(*s: hfiles_and_top_entities sig *)
val hfiles_and_top_entities :
  Common.path -> Database_code.database option -> 
  (Common.filename, Database_code.entity list) Hashtbl.t
(*e: hfiles_and_top_entities sig *)

(*s: all_entities sig *)
(* Will generate extra entities for files, dirs, and also generate
 * an extra entity when have a fullname that is not empty
 *)
val all_entities :
  root:Common.dirname -> Common.filename list -> Database_code.database option->
  Database_code.entity list
(*e: all_entities sig *)

(*s: actual_root_of_db sig *)
val actual_root_of_db : 
  root:Common.path -> Database_code.database -> string
(*e: actual_root_of_db sig *)

(*s: readable_to_absolute_filename_under_root sig *)
val readable_to_absolute_filename_under_root :
  root:Common.path -> string -> string
(*e: readable_to_absolute_filename_under_root sig *)

(*e: model_database_code.mli *)
