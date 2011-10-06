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

}


function test_call_method_dataflow() {
  $o = new B();
  $o->foo(1, 2);
  
  //SKIP: requires dataflow (simple here, but still)
  $o->foo();
}