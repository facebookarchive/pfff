<?php

function foobar() {
}

function foo() {
  bar();
}

function bar() {
}

class A {
  static function mfoo() { foo(); }
  public function mbar() { bar(); }
}

A::mfoo();

$o = new A();
$o->mbar();

class B extends A { }

