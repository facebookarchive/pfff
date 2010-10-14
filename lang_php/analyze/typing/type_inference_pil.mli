

type env = unit (* todo *)

(* Works by side effect on the mutable 't' field of the type_info in
 * the PIL nodes. Use the PIL AST dumper to see those inferred
 * type annotations.
 *)
val infer_types: env -> Pil.program -> unit
