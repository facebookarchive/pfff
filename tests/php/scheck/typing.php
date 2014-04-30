<?php

// Those are just a small set of adhoc typing errors. For a real
// typechecker we should finish and plug typing_php.ml.

function test_string_and_plus($a) {
  //ERROR: Use '.' not '+' to concatenate strings)
  return $a + "foo";
}