(*s: entity_php.mli *)

type id = Id of int

type fullid = filepos
 and filepos = {
  file: Common.filename;
  line: int;
  column: int;
}

type id_kind =
  (* toplevels, which can also be nested *)
  | Function
  | Class
  | Interface
  | StmtList 

  (* only at nested level, inside a class *)
  | Method 
  | ClassConstant
  | ClassVariable
  | XhpDecl
  | StaticMethod 

  | IdMisc

(* Being able to access the definition of a class from a a program requires
 * a global analysis to find where is the class. This should mean
 * that each time some of the analyze_php/ functions need such thing they
 * would need to know about database_php.ml which leads to too many
 * dependencies. Enter 'entity_finder', which is usually build
 * via a closure from a database, but which hides the database to the
 * function using it. See database_php_build.build_entity_finder.
 *)
type entity_finder = (id_kind * string) -> Ast_php.entity list

val str_of_id: id -> string
val str_of_fullid: fullid -> string

val fullid_regexp: string
val fullid_of_string: string -> fullid

val filepos_of_parse_info: Parse_info.parse_info -> filepos

val string_of_id_kind: id_kind -> string

val vof_filepos: filepos -> Ocaml.v
(*x: entity_php.mli *)
(*e: entity_php.mli *)
