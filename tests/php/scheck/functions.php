<?php

function test_call_undefined_function() {
  //ERROR: undefined entity
  undefined_function();
}

function func_foo2($i) {
  echo $i;
}

// Analysis of function calls requires some form of global analysis
// as one needs to get the information about a function usually
// in another file to analyse a call to this function.

function test_function_arity() {
  func_foo2(1);

  //ERROR: not enough arguments
  func_foo2();

  //ERROR: too many arguments
  func_foo2(1,2,3);
}

// this requires to have a good data/php_stdlib/
function test_arity_builtins() {

  //ERROR: not enough arguments
  $x = array_count_values();

  // ok
  $x = array_count_values(array("foo"));

  echo $x;
}

//builtin, now in data/php_stdlib
//function func_num_args() {}
function func_var_args() {
  $args_count = func_num_args();
  echo $args_count;
}

function func_foo_call_var_args() {
  // this is ok
  func_var_args();
  func_var_args(1);
  func_var_args(1,2);

  // same for builtins
  printf("");
  printf("%d", 1);
  printf("%d%d", 1,2);
}


function function_dupe($a) {
  echo $a;
}

//SKIP: not detected at def time :(
function function_dupe($a) {
  echo $a;
}

function test_call_multiply_defined_function() {
  //ERROR: multiply defined entity
  function_dupe(1);

  //TODO should be reported only once, but now we check for undefined entity
  // not only in error_php.ml but also check_variables_php.ml
  //ERROR: multiply defined entity
  function_dupe(1);

  //ERROR: too many args
  function_dupe(1,2);
}

function func_keywords($x, $a = 1, $b = 2) {
  echo $x; echo $a; echo $b;
}

function test_wrong__keywords_arguments() {

  //ERROR: not enough arguments
  func_keywords();

  func_keywords(1);
  func_keywords(1,2);
  func_keywords(1,2,3);
  func_keywords(1, $a = 2, $b = 3);
  func_keywords(1, $a = 2);

  //ERROR: bad keyword argument
  func_keywords(1, $z = 2);

  //ERROR: really really bad keyword argument
  func_keywords(1, $b = 2);
}
