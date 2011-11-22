<?php

class XA {
  public $foo;

  public function test_use_of_undefined() {
    echo $this->foa;
    echo $this->bar;
  }
}


function test_undeclared_var() {
  $foo = 1;
  echo $foa;
  echo $bar;
}