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

  | Other of string
 and notification_kind = 
   | Immediate
   | Consistent
   | Daily

val extract_annotations: string -> annotation list

val vof_annotation: annotation -> Ocaml.v
val str_debug_of_annotation: annotation -> string

(*x: annotation_php.mli *)
(*e: annotation_php.mli *)
