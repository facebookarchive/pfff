
type tag = {
  tag_definition_text: string;
  tagname: string;
  line_number: int;
  (* offset of beginning of tag_definition_text, when have 0-indexed filepos *)
  byte_offset: int; 
}

(* will generate a TAGS file in the current directory *)
val generate_TAGS_file: 
  tags_file: Common.filename -> (Common.filename * tag list) list -> unit

(* internals *)
val mk_tag: string -> string -> int -> int -> tag

val string_of_tag: tag -> string
val header: string
val footer: string
