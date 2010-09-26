
type parsing_stat = {
  mutable correct: int;
  mutable bad: int;
}

type info_item = (Parser_java.token list)
type program2 = 
  (Ast_java.compilation_unit, Ast_java.info list) Common.either * info_item


val parse_java : 
  Common.filename (*javafile*) -> (program2 * parsing_stat)


val print_parsing_stat_list: parsing_stat list -> unit
