
(* This htmlizer uses semantic information such as the number of calls
 * to a function to better colorize entities. So it requires to be
 * passed a db as a parameter.
 *)
val htmlize: 
  Common.filename -> Database_php.database -> string (* raw html *)

(* return one line per line in the original file. You are supposed to
 * enclose those html strings inside a <pre>
 *)
val htmlize_pre: 
  Common.filename -> Database_php.database -> string (* raw html *) list
