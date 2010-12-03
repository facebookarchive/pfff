<?php

// Analysis on function calls requires some form of global analysis
// as one needs to get the information about a function usually
// in another file to analyse a call to this function

function func_foo1() {
  func_foo2(1);

  //ERROR: not enough arguments
  func_foo2();

  //ERROR: too many arguments
  func_foo2(1,2,3);
}

function func_foo2($i) {
  echo $i;
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

}

function func_foo3() {
  //ERROR: undefined entity
  unknown_foo3();
}

function func_dup($a) {
  echo $a;
}

function func_dup($a) {
  echo $a;
}


function func_foo4() {
  //ERROR: multiply defined entity
  func_dup(1);

  func_dup(1);

  func_dup(1,2);
  func_dup(1,2);

}


function func_keywords($x, $a = 1, $b = 2) {
  echo $x; echo $a; echo $b;
}

function func_call_keywords() {

  //ERROR: not enough arguments
  func_keywords();

  func_keywords(1);
  func_keywords(1,2);
  func_keywords(1,2,3);
  func_keywords(1, $a = 2, $b = 3);
  func_keywords(1, $a = 2);

  //ERROR: wrong keyword argument
  func_keywords(1, $z = 2);

  //ERROR: really wrong keyword argument
  func_keywords(1, $b = 2);
}
