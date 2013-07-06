
type annotation =
  | ProvidesModule of Module_js.module_
  | ProvidesLegacy of Module_js.module_
  | RunWhenReady
  | Other of string

(* The returned parse_info is the one associated with the whole comment.
 * We use it in the tag generation.
 *)
val annotations_of_program_with_comments: 
  Parse_js.program_and_tokens -> (annotation * Parse_info.info) list

(* Helper. The string is the string of a comment (with its markers). *)
val extract_annotations: string -> annotation list


