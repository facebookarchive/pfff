<?php

class StaticMethods {
  static public function foo($a) {
    echo $a;
  }
}

function test_undefined_static_method() {
  //ERROR: undefined static method
  StaticMethods::bar();

  //ERROR: undefined class
  UnknownClass::foo();
}

function test_static_method_arity() {
  StaticMethods::foo(1);

  //ERROR: wrong number of arguments
  StaticMethods::foo(1, 2);

  //ERROR: not enough arguments
  StaticMethods::foo();
}

class StaticMethods2 extends StaticMethods {
  static public function bar() {
    // even calling a static method involves a kind of lookup in PHP
    self::foo(1);

    //ERROR: wrong number of arguments
    self::foo(1, 2);
  }

  public function test_use_static_as_non_static() {
    //ERROR: use of static method with $this
    $this->foo(1);
  }
}
