
(*****************************************************************************)
(* types *)
(*****************************************************************************)

type language = 
  | C
  | Cplusplus

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macros_h = 
  ref (Filename.concat Config_pfff.path "/data/cpp_stdlib/macros.h")

let cmdline_flags_macrofile () = [
  "-macros", Arg.Set_string macros_h,
  " <file>";
]

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref true
let verbose_parsing = ref true

(* do not raise Parse_error in parse_cpp.ml, try to recover! *)
let error_recovery = ref true
let show_parsing_error = ref true

let verbose_pp_ast = ref false

let filter_msg = ref false
let filter_classic_passed = ref false
let filter_define_error = ref true

let cmdline_flags_verbose () = [
  "-verbose_parsing_cpp", Arg.Set verbose_parsing, " ";
]

(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_lexer = ref false

let debug_typedef = ref false
let debug_pp = ref false
let debug_pp_ast  = ref false
let debug_cplusplus = ref false

let cmdline_flags_debugging () = [
  "-debug_lexer_cpp",   Arg.Set  debug_lexer , " ";

  "-debug_pp",          Arg.Set  debug_pp, " ";
  "-debug_typedef",     Arg.Set  debug_typedef, "  ";
  "-debug_cplusplus",   Arg.Set  debug_cplusplus, " ";

  "-debug_cpp", Arg.Unit (fun () ->
    debug_pp := true;
    debug_typedef := true;
    debug_cplusplus := true;
  ), " ";
]

(*****************************************************************************)
(* Disable parsing features *)
(*****************************************************************************)

let strict_lexer = ref false

let if0_passing = ref true
let ifdef_to_if  = ref false

let sgrep_mode = ref false
