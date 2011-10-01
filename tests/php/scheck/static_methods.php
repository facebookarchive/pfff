<?php

class StaticMethods {
  static public function foo($a) {
    echo $a;
  }
}

function test_static_method1() {
  StaticMethods::foo(1);

  //ERROR: wrong number of arguments
  StaticMethods::foo(1, 2);

  //ERROR: not enough arguments
  StaticMethods::foo();

  //ERROR: undefined static method
  StaticMethods::bar();

  //ERROR: undefined class
  UnknownClass::foo();
}

class StaticMethods2 extends StaticMethods {
  static public function bar() {
    // even calling a static method involves a kind of lookup in PHP
    self::foo(1);

    //ERROR:
    self::foo(1, 2);
  }
}
