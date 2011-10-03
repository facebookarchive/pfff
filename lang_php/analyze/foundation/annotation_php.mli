(*s: annotation_php.mli *)
type email = string
type unixname = string

type annotation = 
  | Owner of unixname
  | Emails of (email * notification_kind option) list
  | Status of string

  (* deprecated *)
  | Author of string

  | CalledFromPhpsh
  | CalledOutsideTfb
  | CalledDynamically
  | NotDeadCode
  | Have_THIS_FUNCTION_EXPIRES_ON

  | DataProvider of method_callback

  | Other of string
 and notification_kind = 
   | Immediate
   | Consistent
   | Daily
 and method_callback =
   | Method of string
   | MethodExternal of string (* class *) * string

(* main entry point *)
val annotations_of_program_with_comments: 
  Parse_php.program_with_comments -> (Ast_php.info * annotation) list

(* helpers *)
val extract_annotations: string -> annotation list

val vof_annotation: annotation -> Ocaml.v
val str_debug_of_annotation: annotation -> string

(*x: annotation_php.mli *)
(*e: annotation_php.mli *)
