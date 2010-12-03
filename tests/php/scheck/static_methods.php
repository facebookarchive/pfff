<?php

class StaticMethods {
  static public function foo($a) {
    echo $a;
  }
}

function static_foo1() {
  StaticMethods::foo(1);

  //ERROR: wrong number of arguments
  StaticMethods::foo(1, 2);

  //ERROR: not enough arguments
  StaticMethods::foo();

  //ERROR: undefined static method
  StaticMethods::bar();
}