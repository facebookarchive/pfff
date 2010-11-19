<?php

// -------------------------------------------------------------------------
// Basic variable related bugs
// -------------------------------------------------------------------------

//ERROR: unused param
function bad_foo($a) {

  $ok = 1;
  bar($ok);

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
    //ERROR: unused variable, should be declared outide because it's used later
    $nested_var = 1;
  }
  //ERROR: use of undefined variable
  bar($nested_var);

  //ERROR: unused variable
  $match = array();
  // this one error shows the need for more than just counting token
  $matches = array();
  foreach($matches as $match) {
    echo $match;
  }
}

// -------------------------------------------------------------------------
// Algorithm cleverness
// -------------------------------------------------------------------------

// A first algorithm idea proposed by sgrimm was just to count the number
// of occurences for each variables (T_DOLLAR_...)
// and if one variable is mentionned only once, then it's probably a bug.
// It requires just a lexer. Many typos can be detected by this simple 
// algorithm.
function analysis1() {
  //ERROR:
  $occured_only_once_in_this_file = 1;
  //ERROR:
  echo $occured_also_only_once_in_this_file;
}

// If one unused variable in a function happened to be also defined and used
// in another function, then the token-based solution will not detect it.
// You need at least a parser and a basic AST to catch such bugs.
// Moreover it's also ok sometimes to have one variable mentioned
// only once in a file, if it's a parameter of a method in an interface
// definition for instance.
function analysis2() {

  //ERROR: even if $ok was mentionned before, it's no ok anymore
  $ok = 1;
}

// TODO cfg-based algorithm

// TODO liveness-based algorithm

// -------------------------------------------------------------------------
// False positives fix
// -------------------------------------------------------------------------
// My analysis used to have a few false positives because my code was buggy.

function ok_keyword_arguments() {
  // no error for now even if $key appeared as unused. PHP has no
  // keyword arguments so people use such assignation as a kind of
  // comment
  foo($key = 1);
}

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

// -------------------------------------------------------------------------
// TODO
// -------------------------------------------------------------------------

function bad_compact() {

  // this should not generate a warning for now. At some point
  // we want to remove all those ugly compact() but before that, no error.

  //ERROR: todo actually should not generate error
  $foo = 1;
  // this function is horrible. it's the opposite of extract()
  $arr = compact('foo');

  return $arr;
}
