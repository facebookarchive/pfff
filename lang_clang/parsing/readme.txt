This directory is a thin wrapper around 'clang-check --ast-dump' to
make available clang AST to OCaml. See also pfff/lang_cpp/,
pfff/lang_c/. pfff/lang_objc/ which use a C/cpp/C++/ObjectiveC parser
written from scratch.

To be able to start writing analysis in OCaml of C/C++/... code, you
first need to install a 'clang-check' that actually works. Follow the
instructions there
http://clang.llvm.org/docs/LibASTMatchersTutorial.html

Note that the clang in macport doesn't have clang-check :(
Moreover clang-3.2 has a version of clang-check that does not print
everything as a sexp (e.g. typedef are not a sexp) which makes
parsing harder. clang-git seems to be good, but the dumper was evolved, 
and so prefer an old version, e8d411997899a87e1a9f63ad3f52b38e7931687c^
