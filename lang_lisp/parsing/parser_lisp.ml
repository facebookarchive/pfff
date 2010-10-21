
type token =
  | TComment of (Ast_lisp.info)
  | TCommentSpace of (Ast_lisp.info)
  | TCommentNewline of (Ast_lisp.info)

  | TNumber of (string * Ast_lisp.info)
  | TOParen of (Ast_lisp.info)
  | TCParen of (Ast_lisp.info)

  | TUnknown of (Ast_lisp.info)
  | EOF of (Ast_lisp.info)

