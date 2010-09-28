
(* The main function is parse_c_and_cpp. It uses globals in Lexer_Parser and 
 * Parsing_hacks. Especially Parsing_hacks._defs which often comes
 * from a standard.h macro file. Cf also init_defs below.
 *)

type program2 = toplevel2 list
     and toplevel2 = Ast_cpp.toplevel * info_item
    (* the token list contains now also the comment-tokens  *)
     and info_item = (string * Parser_cpp.token list)


(* This is the main function *)
val parse:  
  Common.filename -> (program2 * Parsing_stat_cpp.parsing_stat)

val parse_program:  
  Common.filename -> Ast_cpp.program


val parse_cpp_define_file : 
  Common.filename -> (string, Parsing_hacks.define_body) Common.assoc

val init_defs : Common.filename -> unit


(* return fake program but with good tokens. just use the lexer *)
val parse_tokens:
  Common.filename -> (program2 * Parsing_stat_cpp.parsing_stat)


val tokens:      Common.filename -> Parser_cpp.token list
val tokens_string: string -> Parser_cpp.token list


val parse_simple:                 Common.filename -> Ast_cpp.program
val parse_print_error:            Common.filename -> Ast_cpp.program
val parse_gen: 
    ((Lexing.lexbuf -> Parser_cpp.token) -> Lexing.lexbuf -> 'a) -> string -> 'a

val parse_print_error_heuristic:  
  Common.filename (*cfile*) -> (program2 * Parsing_stat_cpp.parsing_stat)
val parse_c_and_cpp : (* alias of previous func *)
  Common.filename (*cfile*) -> (program2 * Parsing_stat_cpp.parsing_stat)


(* easy way to build complex Ast elements from simple strings *)
val type_of_string      : string -> Ast_cpp.fullType
val statement_of_string : string -> Ast_cpp.statement


val print_commentized       : Parser_cpp.token list -> unit

val threshold_cplusplus : int ref
(* when have a .h, want to know if have to use C or C++ parser *)
val is_problably_cplusplus_file: Common.filename -> bool
