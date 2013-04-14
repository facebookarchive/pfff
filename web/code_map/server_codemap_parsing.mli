
(* internally memoize the highlight information in _hmemo_file *)
val tokens_with_categ_of_file :
  Common.filename ->
  (* (string, Database_code.entity) Hashtbl.t -> *)
  (string * Highlight_code.category option * Common2.filepos) list

(*
val disable_file_in_cache : 
  Common.filename -> unit
*)

(* helpers *)
val use_arity_of_use_count : 
  int -> Highlight_code.use_arity
