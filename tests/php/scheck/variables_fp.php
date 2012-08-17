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
  foo_ref($x, $passed_by_ref_to_funcall);
  echo $passed_by_ref_to_funcall;
}

class RefInStaticMethod {
  public static function static_method($x, &$y) {
    $y = $x;
  }
}

function test_ok_undeclared_when_ref_parameter_static_method() {
  $x = 1;
  RefInStaticMethod::static_method($x, $passed_by_ref_to_static_method_call);
  echo $passed_by_ref_to_static_method_call;
}

class RefInMethod {
  public function method($x, &$y) {
    $y = $x;
  }

  public function test_ok_undeclared_when_ref_parameter_method() {
    $x = 1;
    $this->method($x, $passed_by_ref_to_this_method_call);
    echo $passed_by_ref_to_this_method_call;
  }
}

class TestRefInConstructor {
  public function __construct($x, &$y) {
  }
}

function test_ok_undeclared_when_ref_parameter_new() {
  $o = new TestRefInConstructor(1, $passed_by_ref_to_new);
  echo $passed_by_ref_to_new;
  echo $o;
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

function test_eval_bailout() {
  $used_var_in_eval = 42;
  eval('echo $used_var_in_eval;');
}

function test_eval_var_bailout() {
  $used_var_in_eval_field = 42;
  $x = 'used_var_in_eval_field';
  echo $$x;
}

function ugly() {
  return array('extracted_field' => 2);
}
function test_bailout_extract() {
  extract(ugly());

  // this is ok ...
  echo $extracted_field;

  //SKIP: this is not ok but hard to detect statically in the general case
  echo $wrong_extracted_field_but_skipped;
}

function test_bailout_compact() {
  // this should not generate a warning for now. At some point
  // we want to remove all those ugly compact() but before that, no error.

  //should not generate error
  $compacted = 1;
  // this function is horrible. it's the opposite of extract()
  $arr = compact('compacted');

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
