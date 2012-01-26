two =
  one = 1 // an optional semicolon can be put after
  one + one

two =
  one = 1; // the exact same thing as above
           // can be used to make the code less ambiguous
  one + one

//two =
//  one = 1 // NOT VALID: syntax error because a local declaration
//          // must be followed by an expression
