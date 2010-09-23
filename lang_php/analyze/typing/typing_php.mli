(*s: typing_php.mli *)

(* cf also namespace.ml *)

type environment = (Namespace_php.dnameS, Type_php.phptype) Hashtbl.t


(* !!Annotate via side effects!!. Fill in the type information that
 * was put to None during parsing. I return a program, but really it
 * works by side effect. 
 *)
val annotate_toplevel: 
  environment ref -> Ast_php.toplevel -> Ast_php.toplevel
(*x: typing_php.mli *)
(*e: typing_php.mli *)
