<?php

//*************************************************************************
// Use of undefined variables and unused variables
//*************************************************************************

//ERROR: unused parameter
function test_undefined_and_unused_variables_basic($param, $unused_parameter) {

  $ok = 1;
  // misc1() is defined in common.php
  misc1($ok);

  misc1($param);

  //ERROR: unused variable
  $unused = 1;

  //ERROR: use of undefined variable
  echo $undefined;

  //ERROR: use of undefined variable
  misc1($undefined_bis);
  // such calls would be arguably ok if misc1() was taking its arguments
  // by reference (see also variables_fp.php)
}

function test_unset() {
  //ERROR: unused variable. Yes it's used by unset but this should not count.
  $unset_variable = 1;
  unset($unset_variable);

  $arr = array();
  $arr['field'] = 1;
  unset($arr['field']);

  // you can also unset the field of an object, or even a class variable
  // but I am not sure why you want that ...

  unset($arr[0], $arr[1]);
}

function test_undefined_foreach() {
  $matches = array();

  foreach($matches as $k) {
    echo $k;
  }
  // $k2 is not used but it's ok, as long as one of the key/value is used
  foreach($matches as $k2 => $v) {
    echo $v;
  }

  //SKIP: unused variable, if consider block scope for foreach
  $match = array();
  // note that this error shows also the need for more than just counting token
  foreach($matches as $match) {
    echo $match;
  }

  foreach($matches as &$k) {
    $k = 42;
    echo $k;
  }
}

function test_undefined_list() {
  list($ok, $ok2) = misc1(1);
  echo $ok;
  echo $ok2;

  //ERROR: unused variable
  list($unused, $unused_too) = misc1(1);

  // this is ok, at least of the variable is used
  list($used, $unused_but_ok) = misc1(1);
  echo $used;

  $array = array();
  // this is ok
  list($array['fld1'], $array['fld2']) = misc1(1);

  // this is ok too
  $overriden_by_list = 1;
  list($ok, $overriden_by_list) = misc1(1);
  echo $overriden_by_list;
}

function test_suggest_fix() {
  //ERROR: unused variable, because of the typo in the line below
  $typo_variable = misc1($a);
  //ERROR: error message should suggest and say "do you mean $typo_variable?"
  if ($typo_varable === false) {
    return 1;
  }
  return 2;
}

class TestUndefinedUnusedInMethod {
  public function foo($a) {
    echo $a;
    //ERROR: use of undeclared variable
    echo $undefined;
  }

  public function test_this() {
    return $this;
  }
  public function test_this2() {
    return $this->foo(2);
  }
}

function test_undefined_in_lambda($a) {

  //ERROR: unused variable
  $unused_variable_forgot_pass_to_lambda = "foo";

  $f = (function ($b) use($a) {
    return $a +
           $b +
           //ERROR: use of undeclared variable
           $unused_variable_forgot_pass_to_lambda;
    ;
    });
  return $f;
}

function test_lambda_bis() {
  $a = 1;
  $f = (function ($b) use($a) {
      return $b + $a;
    });
  return $f;
}

class TestLambdaUseThis {
  public function test_lambda($a) {

    $f = (function ($b) use($a, $this) {
        echo $this;
        return $a +
               $b +
               //ERROR: use of undeclared variable
               $use_of_undeclared_variable_in_lambda;
        ;
      });
    return $f;
  }
}

//Test super globals
$x = $_GET['foo'];
echo $x;

//ERROR: not a superglobal
$x = $NOTSUPERGLOBAL['foo'];
echo $x;

function test_super_globals() {
  //ok
  $GLOBALS++;
}

function test_static() {
  static $local_static = 1;
  // ok
  echo $local_static;
}

//*************************************************************************
// Misc regressions
//*************************************************************************

class TestDynProp {
  public $fld;
}
// I was originally not generating the right Ast for dynamic prop access
function test_eval_var_field() {
  $o = new TestDynProp;
  $x = 'fld';
  $o->$x = 1;
  echo $o;
}

// must visit everything
function test_undefined_xhp() {
  //ERROR: use of undefined variable
  return <x:frag>{$undefined}</x:frag>;
}

//*************************************************************************
// False positives fix (see also variables_fp.php)
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

function test_not_sure_what() {
  $a = 1;
  if (isset($a)) {
    return $a;
  }
  return 2;
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
  TestClassVariable::$dbGetters = array();
}

//TODO: test_unused_var_ok_when_assign_ref() { }

//*************************************************************************
// Algorithm cleverness
//*************************************************************************

// A first algorithm idea proposed by sgrimm was just to count the number
// of occurences for each variables (T_DOLLAR_...)
// and if one variable is mentionned only once, then it's probably a bug.
// It requires just a lexer. Many typos can be detected by this simple 
// algorithm.
function analysis1() {
  //ERROR:
  $occured_only_once_in_this_file = 1;
  //ERROR:
  echo $occured_also_only_once_in_this_file;
}

// If one unused variable in a function happened to be also defined and used
// in another function, then the token-based solution will not detect it.
// You need at least a parser and a basic AST to catch such bugs.
function analysis2() {
  //ERROR: even if $ok was mentionned before, it's not ok anymore
  $ok = 1;
}

// Moreover it's also ok sometimes to have one variable mentioned
// only once in a file, if it's a parameter of a method in an interface
// definition for instance.
interface TestUnusedParameterOkInInterface {
  // this is ok, $p is not an unused parameter
  function analysis2bis($p);
}

//*************************************************************************
// TODO
//*************************************************************************

// TODO cfg-based algorithm ??

// TODO liveness-based algorithm
function analysis4() {
  //TODO should be error. useless assignement
  $a = 1;
  $a = 2;
  echo $a;
}

function analysis4bis() {
  $a = 1;
  if (true) { $a = 2; }
  echo $a;
}

function analysis4bis2() {
  //TODO should be error. there is no path where this assignement is useful
  $a = 1;
  if (true) { 
    $a = 2; 
  } else {
    $a = 3;
  }
  echo $a;
}
