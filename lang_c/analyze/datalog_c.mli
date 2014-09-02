
type fact = Datalog_code.fact

type env = {
  scope: string;
  c_file_readable: Common.filename;
  long_format: bool;
  globals: Graph_code.graph;
  globals_renames: Ast_c.name -> Ast_c.name;
  locals: (string * Ast_c.type_ option) list ref;
  facts: fact list ref;
}

type instr

val instrs_of_expr: env -> Ast_c.expr -> instr list

val long_format: bool ref

val facts_of_instr: env -> instr -> fact list
val facts_of_def: env -> Ast_c.toplevel -> fact list
val return_fact: env -> instr -> fact
