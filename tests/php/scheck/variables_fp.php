<?php

//*************************************************************************
// Undefined variables and reference parameters
//*************************************************************************

// Sometimes a variable appears as undeclared but it's because it's
// passed by reference. That means the UseOfUndeclaredVar can't be a
// simple local analysis; it needs to know about the prototype of all the
// functions/methods used :( So to be precise we need to pass an
// entity_finder in scheck variable annotater and checker.

function foo_ref($x, &$y) {
  $y = $x;
  echo $y;
}

function test_ok_undeclared_when_ref_parameter() {
  $x = 1;
  // this is ok to use $y ... I would rather have people declare $y but
  // it's a lost battle so let's accept that and focus on real bugs
  foo_ref($x, $y);
  echo $y;
}

class Foo {
  public static function static_method($x, &$y) {
    $y = $x;
  }
}

function test_ok_undeclared_when_ref_parameter_static_method() {
  $x = 1;
  Foo::static_method($x, $y);
  echo $y;
}

class Foo2 {
  public function method($x, &$y) {
    $y = $x;
  }

  public function test_ok_undeclared_when_ref_paramter_method() {
    $x = 1;
    $this->method($x, $y);
    echo $y;
  }
}

function test_ok_undeclared_when_ref_parameter_method() {
  //$o = new Foo2();
  //$x = 1;
  //SKIP: this requires some dataflow analysis ...
  //$o->method($x, $y);
  //SKIP: same
  //echo $y;
}

//*************************************************************************
// Bailout constructs
//*************************************************************************

function ugly() {
  return array('x1' => 2);
}
function test_bailout_extract() {
  extract(ugly());

  // this is ok ...
  echo $x1;

  //SKIP: this is not ok but hard to detect statically in the general case
  echo $x2;
}

function test_bailout_compact() {
  // this should not generate a warning for now. At some point
  // we want to remove all those ugly compact() but before that, no error.

  //ERROR: todo actually should not generate error
  $foo = 1;
  // this function is horrible. it's the opposite of extract()
  $arr = compact('foo');

  return $arr;
}

//*************************************************************************
// Misc
//*************************************************************************

/* TODO
function test_declared_in_middle_of_expr() {
  if (($v = misc1('')) && misc1($v)) {
    echo $v;
  }
}
*/

function test_ok_undeclared_sscanf() {
  sscanf(PHP_VERSION, '%d', $_PHP_MAJOR_VERSION);
  echo $_PHP_MAJOR_VERSION;
}

function test_isset() {
  if(!isset($foo)) {
    $foo = 1;
    echo $foo;
  }
}
