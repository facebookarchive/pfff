
type program2 = toplevel2 list
    (* the token list contains now also the comment-tokens  *)
   and toplevel2 = Ast_cpp.toplevel * Parser_cpp.token list

exception Parse_error of Parse_info.info

(* This is the main function. It uses _defs below which often comes 
 * from a standard.h macro file 
 *)
val parse:
  ?lang:Flag_parsing_cpp.language ->
  Common.filename -> (program2 * Parse_info.parsing_stat)

val parse_program:  
  Common.filename -> Ast_cpp.program

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_cpp.token list

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Pp_token.define_body) Hashtbl.t ref
(* usually correspond to what is inside your standard.h *)
(* val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref *)

(* todo: init_defs_macros and init_defs_builtins *)
val init_defs : Common.filename -> unit

(* used to extract macros from standard.h, but also now used on C files
 * in -extract_macros to assist in building a macros.h *)
val extract_macros: 
  Common.filename -> (string, Pp_token.define_body) Common.assoc

val tokens:      Common.filename -> Parser_cpp.token list

(* a few helpers *)
val program_of_program2: program2 -> Ast_cpp.program
(*val with_program2: (Ast_c.program -> Ast_c.program) -> program2 -> program2*)
