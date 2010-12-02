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

//builtin
function func_num_args() {}

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
  unknown_foo3();
}

function func_dup() {
}

//function func_dup() {
//}


function func_foo4() {
  func_dup();
}