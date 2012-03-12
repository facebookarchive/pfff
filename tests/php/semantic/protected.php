<?php

class A {
  protected function foo() {
    echo "A::protected::foo";
  }
}

$o = new A();
$o->foo();
