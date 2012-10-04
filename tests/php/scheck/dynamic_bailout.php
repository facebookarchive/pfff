<?php

function test_variable_variable() {

  $x = 1;
  //ERROR: please avoid dynamic features
  ${'foo'.$x} = true;

  foreach (range(0,10) as $x) {
    //ERROR: please avoid dynamic features
    ${'foo'.$x} = true;
  }
  //which generates
  //$foo1 = true;
  //$foo2 = true;
}
