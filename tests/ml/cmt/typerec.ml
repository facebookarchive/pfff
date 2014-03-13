open Pervasives

type variant1 =
  | C1 of int
  | C2 of float

type record1 = {
  fld1: variant1;
  fld2: builtin;
}

type record2 = {
  fld2: record1;
}



type foo = bar
and bar = int

type x = Foo of int list1

 and 'a list1 = 'a * 'a list


type filename = string

type fullid = filepos
 and filepos = {
   file: filename;
 }

(* not enough to expose the bug ... *)
let f x =
  x.file
