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
