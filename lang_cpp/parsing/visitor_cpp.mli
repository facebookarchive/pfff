
open Ast_cpp

(* the hooks *)
type visitor_in = {
  kexpr: expression vin;
  kstmt: statement vin;

  kfieldkind: fieldkind vin;
  kparameter: parameter vin;
  ktypeC: typeC vin;
  kblock_decl: block_declaration vin;
  kcompound: compound vin;
  
  kinfo: info vin;
}
and visitor_out = any -> unit
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out
