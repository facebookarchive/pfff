
type 'tok hooks = {
  kind: 'tok -> Parse_info.token_kind;
  tokf: 'tok -> Parse_info.info;
}

val mk_trees: 'tok hooks -> 'tok list -> Ast_fuzzy.trees
