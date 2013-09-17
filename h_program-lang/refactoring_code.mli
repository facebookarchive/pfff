
type refactoring_kind =
  | SplitMembers
  | AddReturnType of string
  | AddTypeHintParameter of string
  | AddTypeMember of string
  | OptionizeTypeParameter

type position = { 
  file: Common.filename;
  line: int;
  col: int;
}

type refactoring = refactoring_kind * position option

val load: Common.filename -> refactoring list
