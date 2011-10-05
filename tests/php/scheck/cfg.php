<?php

function test_cfg_after_return() {
  
  $a = 1;
  echo $a;
  return 1;
  //ERROR: dead statement
  echo $a;
}

function test_cfg_after_exn() {
  // we have a few code like this. It's an error but a less important one
  throw new Exception();
  //ERROR: dead statement
  return false;
}

function test_cfg_break() {
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

function test_cfg_switch() {
  switch(1) {
    case 1:
      return 2;
    default:
      return 3;
  }
  //ERROR: dead statement, requires a not-so-easy CFG to detect that
  echo 1;
}

function test_cfg_doublecolon() {
  //ERROR: less important too
  return 1;;
}

function test_cfg_ifdef_like() {
  return null;
  
  //ERROR: less important; people abuse return as some kind of #if 0
  $a = 1;
  return $a;
}
