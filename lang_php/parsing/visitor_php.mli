(*s: visitor_php.mli *)
open Ast_php

(*s: type visitor_in *)
(* the hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktop: (toplevel -> unit) * visitor_out -> toplevel  -> unit;
  klvalue: (lvalue -> unit) * visitor_out -> lvalue  -> unit;
  kconstant: (constant -> unit) * visitor_out -> constant  -> unit;
  kscalar: (scalar -> unit) * visitor_out -> scalar  -> unit;
  kstmt_and_def: (stmt_and_def -> unit) * visitor_out -> stmt_and_def  -> unit;
  kencaps: (encaps -> unit) * visitor_out -> encaps -> unit;
  kclass_stmt: (class_stmt -> unit) * visitor_out -> class_stmt -> unit;
  kparameter: (parameter -> unit) * visitor_out -> parameter -> unit;
  kargument: (argument -> unit) * visitor_out -> argument -> unit;
  kcatch: (catch -> unit) * visitor_out -> catch -> unit;

  kobj_dim: (obj_dim -> unit) * visitor_out -> obj_dim -> unit;

  (* xhp: *)
  kxhp_html: (xhp_html -> unit) * visitor_out -> xhp_html -> unit;
  kxhp_attribute: 
    (xhp_attribute -> unit) * visitor_out -> xhp_attribute -> unit;

  kxhp_attr_decl:
    (xhp_attribute_decl -> unit) * visitor_out -> xhp_attribute_decl -> unit;
  kxhp_children_decl:
    (xhp_children_decl -> unit) * visitor_out -> xhp_children_decl -> unit;

  (* Helps abstracting away whether a function/class... is defined in
   * nested way or at the toplevel (e.g. FuncDefNested vs FuncDef)
   *)
  kfunc_def:  (func_def -> unit) * visitor_out -> func_def -> unit;
  kclass_def:  (class_def -> unit) * visitor_out -> class_def -> unit;
  kinterface_def: 
    (interface_def -> unit) * visitor_out -> interface_def -> unit;

  kmethod_def: (method_def -> unit) * visitor_out -> method_def -> unit;

  (* Helps intercepting all the new blocks that in a real language should
   * defined a new scope
   *)
  kstmt_and_def_list_scope: 
    (stmt_and_def list -> unit) * visitor_out -> stmt_and_def list  -> unit;

  kfully_qualified_class_name: 
    (fully_qualified_class_name -> unit) * visitor_out -> 
    fully_qualified_class_name -> unit;
  kclass_name_reference:
    (class_name_reference -> unit) * visitor_out -> 
    class_name_reference -> unit;
  khint_type: (hint_type -> unit) * visitor_out -> hint_type -> unit;
  kqualifier: (qualifier -> unit) * visitor_out -> qualifier -> unit;

  kcomma: (info -> unit) * visitor_out -> info -> unit; 

  kinfo: (info -> unit)  * visitor_out -> info  -> unit;
}
(*e: type visitor_in *)
(*s: type visitor_out *)
and visitor_out = any -> unit
(*e: type visitor_out *)

(*s: visitor functions *)
val default_visitor : visitor_in
(*x: visitor functions *)
val mk_visitor: visitor_in -> visitor_out
(*x: visitor functions *)
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
(*e: visitor functions *)
(*e: visitor_php.mli *)
