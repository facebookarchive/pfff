<?php

//ERROR: unused param
function foo($a) {

  //ERROR: unused variable
  $c = 1;

  //ERROR: use of undefined variable
  echo $b;

  //ERROR: use of undefined variable
  bar($d);

  //ERROR: unused variable. Yes it's used by unset but this should not count
  $memory = 1;
  unset($memory);
}


function ok_keyword_arguments() {

  // no error for now even if $key appeared as unused. PHP has no
  // keyword arguments so people use such assignation as a kind of
  // comment
  foo($key = 1);
}





// My analysis used to have a few false positives because my code
// was buggy.
function ok1() {
  $a = 1;
  if (isset($a)) {
    return $a;
  }
  return 2;
}

function ok2() {
  $db_scb_key = 1;
  if (!isset(A::$dbGetters[$db_scb_key])) {
    return 2;
  }
}


// keyword arguments should be considered even when deeply nested ... hmmm
function ok3() {
  foo(foo($key = 1));
}