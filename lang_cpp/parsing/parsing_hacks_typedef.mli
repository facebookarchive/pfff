
(* We use a list list because the template arguments are passed separately
 * TODO: right now we actually skip template arguments ...
 *)
val find_view_filtered_tokens:
  Token_views_cpp.token_extended list list -> unit
