
let verbose_parsing = ref true
let verbose_lexing = ref false
let verbose_visit = ref true

let cmdline_flags_verbose () = [
  "-no_verbose_parsing_js", Arg.Clear verbose_parsing , "  ";
  "-no_verbose_lexing_js", Arg.Clear verbose_lexing , "  ";
  "-no_verbose_visit_js", Arg.Clear verbose_visit , "  ";
]

let debug_lexer   = ref false

let cmdline_flags_debugging () = [
  "-debug_lexer_js",        Arg.Set  debug_lexer , " ";
]

let show_parsing_error = ref true
