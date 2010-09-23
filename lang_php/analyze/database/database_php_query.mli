(*s: database_php_query.mli *)
open Database_php

(*
val get_functions_ids__of_string: 
  string -> database -> id list
*)

(* general queries, get files or ids *)
val glimpse_get_matching_files: 
  string -> database -> Common.filename list

(* ---------------------------------------------------------------------- *)
(* wrappers around callgraph functions *)
val calltree_callers_of_f: 
  depth:int -> preferences:Callgraph_php.calltree_preferences -> 
  Entity_php.id -> Database_php.database -> 
  Callgraph_php.calltree
val calltree_callees_of_f: 
  depth:int -> preferences:Callgraph_php.calltree_preferences -> 
  Entity_php.id -> Database_php.database -> 
  Callgraph_php.calltree



(* ---------------------------------------------------------------------- *)
val actions: unit -> Common.cmdline_actions
(*x: database_php_query.mli *)
(*e: database_php_query.mli *)
