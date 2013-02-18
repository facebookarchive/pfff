
type compile_commands = Json_type.t

val analyze_make_trace: 
  Common.filename -> compile_commands

val sanitize_compile_commands:
  compile_commands -> compile_commands
