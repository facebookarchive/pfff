(*s: comment_php.mli *)
type comment = 
  | DocBlock of 
      string list (* without the leading ' * ' and '/**' and '*/' *) * 
      bool
  | MultiLineSlashStar of string list (* without the leading ' * ' *)
  | SingleLineSlashStar of string (* without the  enclosing '/* ... */' *)
  | SingleLineSlashSlash of string (* without the '// ' *)
  | OtherStyle of string (* raw *)

(* expect a single line of a comment, not the full comment *)
val strip_comment_marks: string -> string

val parse_comment: string -> comment
val unparse_comment: ?indent:int -> comment -> string

(* if need to add a line in a comment, what should be its leading string.
 * "// ", " * " ? 
 *)
val comment_style_new_line: comment -> string

val index_comment: comment -> (int * string) list

(*e: comment_php.mli *)
