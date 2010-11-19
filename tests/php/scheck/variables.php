<?php

//ERROR: unused param
function foo($a) {

  //ERROR: unused variable
  $c = 1;

  //ERROR: use of undefined variable
  echo $b;
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


