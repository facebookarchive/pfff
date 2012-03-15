<?php

function __builtin__echo($xs) { 
}

class A {
  public function __construct() { }

  public function foo() {
    echo "A::foo\n";
  }
}

class B {
  public function __construct() { }

  public function foo() {
    echo "B::foo\n";
  }
}

class C {
  private $fld;
  public function __construct() {
    $this->fld = new B();
  }
  public function test() {
    var_dump($this);
    $this->fld->foo();
  }
}

function main() {
  $c = new C();
  $c->test();
}

main();
