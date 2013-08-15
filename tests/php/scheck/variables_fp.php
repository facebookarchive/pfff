<?php

//*************************************************************************
// Misc false positives
//*************************************************************************

// My analysis used to have a few false positives because my code was buggy.

function test_unused_var_ok_when_keyword_arguments() {
  // no error for now even if $key appeared as unused. PHP has no
  // keyword arguments so people use such assignation as a kind of
  // comment
  misc1($key = 1);
}

// keyword arguments should be considered even when deeply nested ... hmmm
function test_unused_var_ok_when_keyword_arguments_bis() {
  misc1(misc1($key = 1));
}

function test_undefined_ok_if_isset() {
  if (isset($a)) {
    return 1;
  }
  return 2;
}

function test_isset_implicit_declaration() {
  if(isset($a)) {
    // TODO: should allow that, we should analyze guards
    //return $a;
  }
  if(!isset($isset_var)) {
    $isset_var = 1;
    echo $isset_var;
  }
}

function test_isset_considered_as_a_use() {
  //ERROR: unused local variable
  $a = "foo";
  //todo? consider this as a use? https://github.com/facebook/pfff/issues/42
  echo isset($a);
}

function test_undefined_ok_if_empty() {
  if (empty($undefined_variable_but_arg_to_empty_so_ok)) {
    return 1;
  }
}

class TestClassVariable {
  public static $dbGetters;
}
function test_class_variables() {
  $db_scb_key = 1;
  if (!isset(TestClassVariable::$dbGetters[$db_scb_key])) {
    return 2;
  } 
  // checks for use of undefined variable are restricted to local vars
  // not class variables of object members. See check_classes_php.ml for that.
  echo TestClassVariable::$dbGetters;
  $dyn = 'TestClassVariable';
  echo $dyn::$dbGetters;

  TestClassVariable::$dbGetters = array();
}

//TODO: test_unused_var_ok_when_assign_ref() { }

function test_unused_var_ok_in_catch() {
  try {
  // this is ok if the variable name has a known name like $unused, $exn, etc
  } catch (Exception $exn) {
  }
}

// this used to raise false positives when I was using the old visitor-based
// variable checker. Switching to Ast_php_simple and an env-based recursive
// approach solved the problem for free.
function test_declared_in_middle_of_expr() {
  // it's ok to use the variable in the right of &&.
  if (($v = misc1('')) && misc1($v)) {
    // it's also ok to use it here
    echo $v;
  }

  //ERROR: use == or add another set of parens around the assignment
  if ($v = misc1('')) {
    echo $v;
  }
}

// this used to raise a 'use of undefined variable $a' error
// because the list() construct was pattern matched only when at
// the toplevel of an expression. The env-based approach instead
// of visitor-based approach to check_variables_php.ml solved this for free
function test_list_in_middle_of_expr() {
  list($a, $b) = misc1(1) or misc1(2);
  echo $a;
}

function test_list_and_at() {
  @(list($ok1, $ok2) = misc1(1));
  echo $ok1;
}

function test_ok_undeclared_sscanf() {
  sscanf(PHP_VERSION, '%d', $_PHP_MAJOR_VERSION);
  echo $_PHP_MAJOR_VERSION;
}

function take_variable_number_of_args() {
  misc1(func_get_args());
}

// this used to raise a "unused variable" because we were not
// visiting all the arguments when the definition of the corresponding
// function didn't have any parameter
function test_unused_and_varargs_function() {
  $avar = 1;
  take_variable_number_of_args($avar);
}

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
  //this should not generate an "unused variable" error
  $used_var_in_eval = 42;
  //ERROR: please avoid dynamic code
  eval('echo $used_var_in_eval;');
}

function test_eval_var_bailout() {
  $used_var_in_eval_field = 42;
  $x = 'used_var_in_eval_field';
  //ERROR: please avoid dynamic code
  echo $$x;
}

function ugly() {
  return array('extracted_field' => 2);
}
function test_bailout_extract() {
  //ERROR: please avoid dynamic code
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
  //ERROR: please avoid dynamic code
  $arr = compact('compacted');

  return $arr;
}

