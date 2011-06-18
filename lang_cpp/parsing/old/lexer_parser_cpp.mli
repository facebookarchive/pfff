
val is_typedef : string -> bool

val enable_typedef  : unit -> unit
val disable_typedef : unit -> unit
val is_enabled_typedef : unit -> bool

val add_ident   : string -> unit
val add_typedef : string -> unit
val add_typedef_root : string -> unit

val lexer_reset_state : unit -> unit

val save_typedef_state : unit -> unit
val restore_typedef_state : unit -> unit

(* private *)
type identkind = TypeDefI | IdentI
val _typedef : (string, identkind) Common.scoped_h_env ref
val _old_state : (string, identkind) Common.scoped_h_env ref
val _handle_typedef : bool ref

val new_scope : unit -> unit
val del_scope : unit -> unit

type context = 
 ...
type lexer_hint = { 
  mutable context_stack: context Common.stack;
 }
val default_hint : unit -> lexer_hint

val is_top_or_struct : context -> bool

val _lexer_hint : lexer_hint ref
val current_context: unit -> context
val push_context: context -> unit
val pop_context: unit -> unit

