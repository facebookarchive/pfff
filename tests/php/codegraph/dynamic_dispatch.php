<?php

abstract class AX {
  public function foo() {
    $this->abstr();
  }
  public function abstr();
}

abstract class BX extends AX {
  public function bar() {
    $this->foo();
  }
}

class CX extends BX {
  public function foo() {
  }
  public function abstr() {
  }
}