<?php

// I don't understand late static binding, but I need to parse it at least ...
class A {
  public function getConstant() {
    return static::A_CONSTANT;
  }
}

