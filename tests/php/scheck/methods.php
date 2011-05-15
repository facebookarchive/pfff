<?php

class A {
  public function foo($param1, $param2) {
    echo $param1;
    echo $param2;
  }
}

class B extends A {
  // this is actually ok. The parameters are not used but in a method
  // context with inherited methods, you may not need all the parameters
  // of your parents
  public function foo($param1, $param2) {
  }
}