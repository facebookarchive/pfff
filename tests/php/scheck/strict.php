<?php

// those errors are reported only when we run scheck with -strict

function test_strict_var() {

  $vars = array();
  foreach($vars as $var) {
    misc1($var);
    //ERROR: unused variable, should be declared outide because it's used later
    $nested_var = 1;
  }
  //ERROR: use of undefined variable
  misc1($nested_var);

}
