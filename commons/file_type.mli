
type file_type = 
  | PL of pl_type
  | Obj of string
  | Binary of string
  | Text of string
  | Doc of string
  | Media of media_type
  | Archive of string
  | Other of string

 and pl_type = 
  | ML of string | Haskell of string
  | Makefile
  | Script of string
  | C | Cplusplus | Java | Csharp
  | Scheme | Lisp 
  | Elisp
  | Perl | Python | Ruby
  | Erlang
  | Beta
  | Web of webpl_type
  | R of string
  | Asm
  | Thrift
  | MiscPL of string

   and webpl_type = 
     | Php of string
     | Js
     | Css
     | Html | Xml | Json
     | Sql

 and media_type =
   | Sound of string
   | Picture of string
   | Video of string


val file_type_of_file: 
  Common.filename -> file_type

val is_textual_file: 
  Common.filename -> bool

(* specialisations *)
val webpl_type_of_file: 
  Common.filename -> webpl_type option

(*
val string_of_pl: pl_kind -> string
*)
