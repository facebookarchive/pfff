(*s: dataflow_php_array.mli *)

(* position information about a token of the AST *)
type info = Entity_php.filepos

type usage = (node, leaf) Common.tree
 and leaf = 
   | Get of info
 and node = 
   | Assign of info
   (* interprocedural *)
   | Return of info
   | Call of info

val track_function_result: 
  string -> Database_php.database -> usage

val print_usage: 
  usage -> unit

(*e: dataflow_php_array.mli *)
