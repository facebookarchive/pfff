  char = parser
  | x = { Rule.alphanum_char } -> x
  | x = "-" -> x
  // was "{" but bad I think
  | x = "\{" -> x
  | x = "}" -> x
