
(*****************************************************************************)
(* convenient globals *)
(*****************************************************************************)

let path = ref
  (try (Sys.getenv "PFFF_HOME")
    with Not_found-> "/home/pad/pfff"
  )

(*****************************************************************************)
(* macros *)
(*****************************************************************************)

let macros_h = ref (Filename.concat !path "/data/cpp_stdlib/macros.h")

let cmdline_flags_macrofile () = [
  "-macros", Arg.Set_string macros_h,
  " <file>";
]

(*****************************************************************************)
(* show *)
(*****************************************************************************)

(*****************************************************************************)
(* verbose *)
(*****************************************************************************)

let verbose_lexing = ref false
let verbose_parsing = ref true

let verbose_cpp_ast = ref false

let filter_msg = ref false

let filter_classic_passed = ref false
let filter_define_error = ref false

let cmdline_flags_verbose () = [
  
]

(*****************************************************************************)
(* debugging *)
(*****************************************************************************)

let debug_etdt = ref false
let debug_typedef = ref false
let debug_typedef_location = ref false
let debug_lexer = ref false

let debug_pp = ref false
let debug_cplusplus = ref false
let debug_cpp_ast  = ref false

let cmdline_flags_debugging () = [
  "-debug_lexer_cpp",   Arg.Set  debug_lexer , " ";

  "-debug_pp",          Arg.Set  debug_pp, " ";
  "-debug_etdt",        Arg.Set  debug_etdt , "  ";
  "-debug_typedef",     Arg.Set  debug_typedef, "  ";

  "-debug_cplusplus",   Arg.Set  debug_cplusplus, " ";
]

(*****************************************************************************)
(* Disapble parsing feature *)
(*****************************************************************************)

let add_typedef_root = ref false

let if0_passing = ref true

let ifdef_to_if  = ref false
