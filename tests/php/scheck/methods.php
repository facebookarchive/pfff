<?php

class A {
  public function __construct() {
  }

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



class C extends B {

  function test_call_undefined_method() {
    //ERROR: undefined method
    $this->bar();
  }

  public function test_method_arity() {
    $this->foo("a", "b");

    //ERROR: not enough arguments
    $this->foo();
  }

  public function test_call_method_insensitive() {
    //SKIP: ok for now PHP is case insensitive and I focus on real errors first
    $this->FoO(1, 2);
  }

  public function test_non_static_method() {
    //ERROR: calling non static method with a qualifier
    self::foo();
  }
  function __construct() {
    //this is the only exception, can call non static via qualifier for that
    parent::__construct();
  }

}

class D {
  public $delegate;
  public function __construct() {
    $this->delegate = new A();
  }
  public function __call($method, $args) {
    return call_user_func_array(array($this->delegate, $method), $args);
  }

  public function test__call() {
    $this->foo(1, 2);

    //SKIP: this should be an error, but we bailout for now
    $this->bar();
  }
}

function test_call_method_dataflow() {
  $o = new B();
  $o->foo(1, 2);
  
  //SKIP: requires dataflow (simple here, but still)
  $o->foo();
}

abstract class Ab {
  public function test_undefined_abstract_method() {

    //ERROR: UndefinedMethodButMaybeOkBecauseAbstract
    $this->bar();
  }
}