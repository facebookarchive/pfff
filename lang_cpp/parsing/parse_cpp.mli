
(* The main function is parse_c_and_cpp. It uses globals in Lexer_Parser and 
 * and also _defs below which often comes from a standard.h macro file. 
 * cf also init_defs below.
 *)

type program2 = toplevel2 list
   and toplevel2 = Ast_cpp.toplevel * info_item
    (* the token list contains now also the comment-tokens  *)
     and info_item = (string * Parser_cpp.token list)

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Pp_token.define_body) Hashtbl.t ref
(* usually correspond to what is inside your standard.h *)
(* todo:val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref*)

(* todo: init_defs_macros and init_defs_builtins *)
val init_defs : Common.filename -> unit

(* This is the main function *)
val parse:  
  Common.filename -> (program2 * Statistics_parsing.parsing_stat)

val threshold_cplusplus : int ref
(* when have a .h, want to know if have to use C or C++ parser *)
val is_problably_cplusplus_file: Common.filename -> bool

val parse_program:  
  Common.filename -> Ast_cpp.program

(* ---------------------------------------------------------------------- *)
(* used to extract macros from standard.h, but also now from regular C files
 * in -extract_macros to later feed an automatically build standard.h *)
val extract_macros: 
  Common.filename -> (string, Pp_token.define_body) Common.assoc

(* ---------------------------------------------------------------------- *)
(* return fake program but with good tokens. just use the lexer *)
val parse_tokens:
  Common.filename -> (program2 * Statistics_parsing.parsing_stat)

val tokens:      Common.filename -> Parser_cpp.token list
val tokens_string: string -> Parser_cpp.token list


val parse_simple:                 Common.filename -> Ast_cpp.program
val parse_print_error:            Common.filename -> Ast_cpp.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_cpp.token) -> Lexing.lexbuf -> 'a) -> string -> 'a

val parse_print_error_heuristic:  
  Common.filename (*cfile*) -> (program2 * Statistics_parsing.parsing_stat)
val parse_c_and_cpp : (* alias of previous func *)
  Common.filename (*cfile*) -> (program2 * Statistics_parsing.parsing_stat)


(* ---------------------------------------------------------------------- *)
(* Easy way to build complex Ast elements from simple strings.
 * Can also be useful when called from the ocaml toplevel to test. 
 *)
val type_of_string      : string -> Ast_cpp.fullType
val statement_of_string : string -> Ast_cpp.statement


(* ---------------------------------------------------------------------- *)
(* a few helpers *)
val print_commentized       : Parser_cpp.token list -> unit

val program_of_program2: program2 -> Ast_cpp.program
(*val with_program2: (Ast_c.program -> Ast_c.program) -> program2 -> program2*)
