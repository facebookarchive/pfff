(* 
 * See lang_cpp/parsing/ast_cpp.ml.
 * 
 * There is no Concrete Syntax Tree for C. If you want a real
 * AST then see lang_c/analyze/ast_c_simple.ml which is build from
 * an ast_cpp.ml tree.
 * 
 *)
type program = Ast_cpp.program
