
type 'tok hooks = {
  kind: 'tok -> Parse_info.token_kind;
  tokf: 'tok -> Parse_info.info;
}

val comment_before:
 'a hooks -> Parse_info.info -> 'a list -> Parse_info.info option

val comment_after:
 'a hooks -> Parse_info.info -> 'a list -> Parse_info.info option
