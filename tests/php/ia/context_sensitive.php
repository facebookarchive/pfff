<?php

function foo($x) {
  var_dump($x);
}

// the interpreter will run those multiple funcalls with different contexts
foo(42);
foo("bar");
