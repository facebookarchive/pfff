<?php

function foo($o) {
  $o['name'] = "foo";
  bar();
}

function bar($o) {
  echo $o['name'];
}

class A {
  public $fld;

  public function methA() {
    foo();
  }
}

class B extends A {
  public function methA() {
    bar();
    echo $this->fld;
  }
  public function methB() {
    $this->fld = 1;
  }
}

interface I {
  public function inter();
}

class C extends B implements I {
  public function inter() {
    bar();
  }
}

function test_long_field($o) {
  echo $o["this is a'valid field too"];
}