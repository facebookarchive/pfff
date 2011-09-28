<?php

function foo_ref($x, &$y) {
  $y = $x;
  echo $y;
}

class Foo {
  public static function foo_ref2($x, &$y) {
    $y = $x;
  }
  public static function foo_ref3($x, &$y) {
    $y = $x;
  }
}

function test_fp_undeclared() {
  $x = 1;
  foo_ref($x, $y);
  echo $y;
}

function test_fp_undeclared2() {
  $x = 1;
  Foo::foo_ref2($x, $y);
  echo $y;
}