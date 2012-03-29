<?php

trait T {
  public static function foo() {
  }
}
class A_traits {
  use T;
}

class B_traits extends A_traits  {
}

// This is ok
B_traits::foo();
