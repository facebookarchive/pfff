
type refactoring = { 
  file: Common.filename;
  line: int;
  col: int;
  action: refactoring_kind;
}
  and refactoring_kind =
    | AddReturnType of string
    | AddTypeHintParameter of string
    | AddTypeMember of string
    | OptionizeTypeParameter

val load: Common.filename -> refactoring list
