
val build:
  ?verbose:bool -> 
  ?graph_code_java:Graph_code.graph option ->
  Common.dirname -> Common.filename list ->
  Graph_code.graph


(* bytecode <-> java mapping *)
type bytecode_class_name = {
  package: string list;
  baseclass: string;
  nested_or_anon: dollar_suffix list
}
  and dollar_suffix =
  | DollarNestedClass of string
  | DollarAnonClass of (*int*) string

(* ex: "Foo.Bar$FooBar$1" -> { package = "Foo"; baseclass = "Bar"; ... } *)
val bytecode_class_name_of_string: string -> bytecode_class_name
