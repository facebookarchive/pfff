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

  $vars = array();
  foreach($vars as $var) {
    bar($var);
    //ERROR: unused variable, should be declared first outide foreach!
    $nested_var = 1;
  }

  //ERROR: use of undefined variable
  bar($nested_var);
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

function ok_compact() {

  // this should not generate a warning for now. At some point
  // we want to remove all those ugly compact() but before that, no error.


  //ERROR: todo actually should not generate error
  $foo = 1;
  // this function is horrible. it's the opposite of extract()
  $arr = compact('foo');

  return $arr;
}