open Foo1

module A = Foo2

let x = 
  A.bar();
  Foo3.bar();
  pr2 (x.A.bar);
  ()

