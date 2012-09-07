(*s: completion2.mli *)

val build_completion_defs_index : 
  Database_code.entity list -> Big_grep.index
(*x: completion2.mli *)

val my_entry_completion_eff :
  callback_selected:
   (GEdit.entry -> string -> string -> Database_code.entity -> bool) ->
  callback_changed:(string -> unit) -> 
  (unit -> Big_grep.index) ->
  GEdit.entry
(*e: completion2.mli *)
