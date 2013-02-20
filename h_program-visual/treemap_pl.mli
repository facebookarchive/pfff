
val color_of_source_archi: 
  Archi_code.source_archi -> Simple_color.emacs_color

val color_of_webpl_type:
  File_type.webpl_type -> Simple_color.emacs_color

val anamorphic_diviser_of_file:
  root:Common.dirname ->
  Common.filename -> float

val no_filter_file: Common.filename -> bool
val ex_filter_file: Common.filename -> bool
val ocaml_filter_file: Common.filename -> bool
val ocaml_mli_filter_file: Common.filename -> bool
val pad_filter_file: Common.filename -> bool
val php_filter_file: Common.filename -> bool

(* default treemap *)
val code_treemap: 
  filter_file: (Common.filename -> bool) ->
  Common2.path list ->
  (string, Common.filename * int) Treemap.treemap
