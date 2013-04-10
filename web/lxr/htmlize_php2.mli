
(* todo: bad, should have 'css' type *)
val style: string

val htmlize :
  hook_token:(string ->
              Parser_php.token ->
              Highlight_code.category option ->
              ([< HTML5_types.span_content_fun > `PCDATA `Span ] as 'a) Eliom_pervasives.HTML5.M.elt) ->
  Common.filename -> Database_php.database -> 'a Eliom_pervasives.HTML5.M.elt list

val htmlize_with_headers :
  hook_token:(string ->
              Parser_php.token ->
              Highlight_code.category option ->
              [< HTML5_types.span_content_fun > `PCDATA `Span ] Eliom_pervasives.HTML5.M.elt) ->
  Common.filename -> Database_php.database -> [> `Html ] Eliom_pervasives.HTML5.M.elt
