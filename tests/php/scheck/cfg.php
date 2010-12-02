<?php

function cfg_foo() {
  
  $a = 1;
  echo $a;
  return 1;
  //ERROR: dead statement
  echo $a;
}

function cfg_foo2() {
  // we have a few code like this. It's an error but a less important one
  throw new Exception();
  //ERROR: dead statement
  return false;
}

function cfg_foo3() {
  // we have a few code like this. It's an error but a less important one
  switch(1) {
    case 1:
      return 2;
      //ERROR: dead statement
      break;
    default:
      return 3;
  }
}

function cfg_foo4() {
  switch(1) {
    case 1:
      return 2;
    default:
      return 3;
  }
  //ERROR: dead statement, requires a not-so-easy CFG to detect that
  echo 1;
}

function cfg_foo5() {
  //ERROR: less important too
  return 1;;
}

function cfg_foo6() {
  return null;
  
  //ERROR: less important; people abuse return as some kind of #if 0
  $a = 1;
  return $a;
}
