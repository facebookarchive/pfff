(*
Normally to make ocaml and python cooperate, you should use
thrift. Can do Json/sexp but that's low level interoperability.
We prefer typed interfaces. Pb is that if you do  a compiler
front-end, that means AST, which means Algebric data types which
are not supported by thrift. So first need to solve this problem.
One way is to keep the AST, and make a tool to transform
this AST into something thrift can grok, like classes.
the problem is that thrift can not even accept classes
and recursive types ... so hopeless to use thrift
for representing complex data structures.

Still if at some point you want to use it, you may leverage
ocaml.ml and ocamltarzan (and maybe even camlmix) to automatically
generate .thrift files from type definitions.

*)
