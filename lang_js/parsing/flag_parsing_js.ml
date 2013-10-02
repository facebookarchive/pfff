
let verbose_lexing = ref false
let verbose_parsing = ref true
let exn_when_lexical_error = ref true

let debug_lexer   = ref false

let show_parsing_error = ref true
(* Do not raise an exn when a parse error but use NotParsedCorrectly.
 * Now that the JS parser is quite complete, it's better to set 
 * error_recovery to false by default and raise a true ParseError exn.
 *)
let error_recovery = ref false


let cmdline_flags_verbose () = [
  "-no_verbose_parsing_js", Arg.Clear verbose_parsing , "  ";
  "-no_verbose_lexing_js", Arg.Clear verbose_lexing , "  ";
]
let cmdline_flags_debugging () = [
  "-debug_lexer_js",        Arg.Set  debug_lexer , " ";
]

