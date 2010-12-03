(*s: ast_entity_php.mli *)
(*x: ast_entity_php.mli *)
open Ast_php

type id_ast = 
  | Function of func_def
  | Class of class_def
  | Interface of interface_def
  | StmtList of stmt list

  | Method of method_def

  | ClassConstant of class_constant
  | ClassVariable of class_variable * modifier list

  | XhpDecl of xhp_decl

  | Misc of info list

val toplevel_to_idast: toplevel -> id_ast

(* Being able to access the definition of a class from a a program requires
 * a global analysis to find where is the class. This should mean
 * that each time some of the analyze_php/ functions need such thing they
 * would need to know about database_php.ml which leads to too many
 * dependencies. Enter 'entity_finder', which is usually build
 * via a closure from a database, but which hides the database to the
 * function using it. See database_php_build.build_entity_finder.
 * 
 * update: I now return a list of id_ast instead of a single id_ast
 * and raising a Not_found or Multi_found. The rational is that
 * the caller knows better what to do when there are multiple
 * matching definitions for an entity. For instance for 
 * better error message having just Multi_found is not enough.
 * The caller needs for instance to show two witnesses.
 *)
type entity_finder = (Entity_php.id_kind * string) -> id_ast list

(*e: ast_entity_php.mli *)
