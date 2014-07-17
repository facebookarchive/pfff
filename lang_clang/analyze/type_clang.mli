
type type_clang = 
  | Builtin of string
  | Typename of string

  (* pointer or array *)
  | Pointer of type_clang
  | Function of type_clang

  | StructName of string
  | UnionName of string
  | EnumName of string

  | AnonStuff
  | TypeofStuff

  | Other of Parser_clang.token list

val string_of_type_clang: type_clang -> string

val builtin_types: string Common.hashset

val tokens_of_brace_sexp: 
  bool (* typedef deps? *) -> Ast_clang.loc -> Ast_clang.sexp -> 
  Parser_clang.token list

val tokens_of_paren_sexp: 
  Ast_clang.loc -> Ast_clang.sexp -> 
  Parser_clang.token list

val expand_typedefs: 
  (string, type_clang) Hashtbl.t -> type_clang -> type_clang

val type_of_tokens: 
  Ast_clang.loc -> Parser_clang.token list -> type_clang


