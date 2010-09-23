
val _handle_typedef : bool ref

val enable_typedef  : unit -> unit
val disable_typedef : unit -> unit
val is_enabled_typedef : unit -> bool



(* private *)
type identkind = TypeDefI | IdentI
val _typedef : (string, identkind) Common.scoped_h_env ref

val add_ident   : string -> unit
val add_typedef : string -> unit
val add_typedef_root : string -> unit

val new_scope : unit -> unit
val del_scope : unit -> unit

val is_typedef : string -> bool

val lexer_reset_typedef : unit -> unit

val _old_state : (string, identkind) Common.scoped_h_env ref
val save_typedef_state : unit -> unit
val restore_typedef_state : unit -> unit


type context = 
  | InTopLevel
  | InFunction
  | InClassStruct of string
  | InStructAnon
  | InTemplateParam
  | InParameter
  | InInitializer
  | InEnum

val is_top_or_struct : context -> bool

type lexer_hint = { 
  mutable context_stack: context Common.stack;
 }

val _lexer_hint : lexer_hint ref
val current_context: unit -> context
val push_context: context -> unit
val pop_context: unit -> unit

val default_hint : unit -> lexer_hint
