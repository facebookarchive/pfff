<?php

function test_dead_after_return() {
  $a = 1;
  echo $a;
  return 1;
  //ERROR: dead statement
  echo $a;
}

function test_dead_after_exn() {
  // we have a few code like this. It's an error but a less important one
  throw new Exception();
  //ERROR: dead statement
  return false;
}

function test_dead_break() {
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

function test_dead_after_switch() {
  switch(1) {
    case 1:
      return 2;
    default:
      return 3;
  }
  //ERROR: dead statement, requires a not-so-easy CFG to detect that
  echo 1;
}

function test_dead_doublecolon() {
  //ERROR: less important too
  return 1;;
}

function test_dead_ifdef_like() {
  return null;
  
  //ERROR: less important; people abuse return as some kind of #if 0
  $a = 1;
  return $a;
}

function test_dynamic_break() {
  while(1) {
    $x = 1;
    //ERROR: dynamic break
    break $x;
  }
}

function test_dynamic_break_ok() {
  while(1) {
    //this is ok
    break 1;
  }

  while(1) {
    //this is ok too
    break (1);
  }
}

function test_for_continue_ok() {
  // this is ok, there is no deadcode, the $i++ is reached by the 'continue'
  for ($i = 0; $i < 3; $i++) {
    if($i === 1) {
      continue;
    }
    return 1;
  }
}
