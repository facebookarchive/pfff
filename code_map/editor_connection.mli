(*s: editor_connection.mli *)

val emacsclient_path: string ref

val open_file_in_current_editor: file:string -> line:Model2.line -> unit
(*e: editor_connection.mli *)
