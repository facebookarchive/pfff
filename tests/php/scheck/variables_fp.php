<?php

// sometimes a variable appaears as undeclared but it's because it's
// passed by reference. That means the UseOfUndeclaredVar can't be a
// simple local analysis; it needs to know about the prototype of the
// functions/methods used.

function foo_ref($x, &$y) {
  $y = $x;
  echo $y;
}

function test_fp_undeclared() {
  $x = 1;
  // this is ok ... I would rather have people declare $y but it's
  // a lost battle so let's accept that and focus on real bugs
  foo_ref($x, $y);
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

function test_fp_undeclared2() {
  $x = 1;
  Foo::foo_ref2($x, $y);
  echo $y;
}

function test_fp_undeclared3() {
  sscanf(PHP_VERSION, '%d', $_PHP_MAJOR_VERSION);
  echo $_PHP_MAJOR_VERSION;
}

function ugly() {
  return array('x1' => 2);
}
function test_bailout3() {
  extract(ugly());

  // this is ok ...
  echo $x1;

  //SKIP: this is not ok but hard to detect statically in the general case
  echo $x2;
}

class Foo2 {
  public function method($x, &$y) {
    $y = $x;
  }

  public function test_fb_undeclared4() {
    $x = 1;
    $this->method($x, $y);
    echo $y;
  }
}

function test_fb_undeclared5() {
  //$o = new Foo2();
  //$x = 1;
  //SKIP: this requires some dataflow analysis ...
  //$o->method($x, $y);
  //SKIP: same
  //echo $y;
}

function test_isset() {
  if(!isset($foo)) {
    $foo = 1;
    echo $foo;
  }
}