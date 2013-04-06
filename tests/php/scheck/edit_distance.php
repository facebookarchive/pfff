<?php

class XA {
  public $foo;

  public function test_use_of_undefined() {
    //ERROR: 
    echo $this->foa;
    //ERROR: 
    echo $this->bar;
  }
}


function test_undeclared_var() {
  $foo = 1;
  //ERROR: 
  echo $foa;
  //ERROR: 
  echo $bar;
  echo $foo;
}