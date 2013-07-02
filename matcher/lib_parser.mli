
type token_kind =
  | LPar
  | RPar
  | LBrace
  | RBrace
  | Other

type 'tok hooks = {
  kind: 'tok -> token_kind;
  tokf: 'tok -> Parse_info.info;
}

val mk_trees: 'tok hooks -> 'tok list -> Ast_fuzzy.trees
