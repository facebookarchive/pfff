<?php

class A {
  public function foo() {
    echo "A::foo()\n";
  }
}

class B extends A {
  public function __call($f, $args) {
    echo "B::__call($f, $args)\n";
  }
}

$o = new B();
$o->foo();
$o->bar();
