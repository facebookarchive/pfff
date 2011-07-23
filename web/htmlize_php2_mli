
(* todo: bad, should have 'css' type *)
val style: string

val htmlize: 
  hook_token:(string ->
    Parser_php.token ->
      Highlight_code.category option ->
        ([< Xhtmltypes.span_content > `PCDATA `Span ] as 'a)
          XHTML.M.elt) ->
  Common.filename -> Database_php.database -> 
 'a XHTML.M.elt list

val htmlize_with_headers: 
  hook_token:(string ->
    Parser_php.token ->
      Highlight_code.category option ->
        ([< Xhtmltypes.span_content > `PCDATA `Span ] as 'a)
          XHTML.M.elt) ->
  Common.filename -> Database_php.database ->
  XHTML.M.html
