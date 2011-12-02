<?php

function foo() {
  bar();
}

function bar() {
  
}

class A {
  static function foo() { }
  public function bar() { }
}

A::foo();

$o = new A();
$o->bar();

