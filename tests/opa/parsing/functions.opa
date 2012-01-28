// defining function f with the two parameters x and y
function f(x,y) {
  x + y + 1
}

// functions call be defined locally, just like other values
two =
  function f(x) { x + 1 }
  f(1)


// The old syntax of OPA was making parsing with yacc very difficult.
// With:
//    two =
//       f(x) = x + 1
//       f(1)
// the parser has no way without a big lookahead to realize
// with the first open parenthesis that we start a function definition
// and not a function call. One need to see the '=' far away to realize that.
