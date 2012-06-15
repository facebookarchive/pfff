<?php

//*************************************************************************
// Use of undefined variables and unused variables
//*************************************************************************

//ERROR: unused parameter
function test_undefined_and_unused_variables_basic($a) {

  $ok = 1;
  misc1($ok);

  //ERROR: unused variable
  $c = 1;

  //ERROR: use of undefined variable
  echo $b;

  //ERROR: use of undefined variable
  misc1($d);
  // such calls would be arguably ok if misc1() was taking its arguments
  // by reference (see also variables_fp.php)

  //ERROR: unused variable. Yes it's used by unset but this should not count.
  $memory = 1;
  unset($memory);

  //ERROR: unused variable
  $match = array();
  // note that this error shows also the need for more than just counting token
  $matches = array();
  foreach($matches as $match) {
    echo $match;
  }
}

class TestUndefinedUnusedInMethod {
  public function foo($a) {
    //ERROR: unused variable, because of the typo in the line below
    $im_service = misc1($a);
    //ERROR: use of undefined variable, typo
    if ($im_servce === false) {
      return 1;
    }
    return 2;
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
  $c = "foo";

  $f = (function ($b) use($a) {
    return $a +
           $b +
           //ERROR: use of undeclared variable
           $c;
    ;
    });
  return $f;
}

class TestLambda {
  public function test_lambda($a) {

    $f = (function ($b) use($a, $this) {
        echo $this;
        return $a +
               $b +
               //ERROR: use of undeclared variable
               $c;
        ;
      });
    return $f;
  }
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
}


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
// Moreover it's also ok sometimes to have one variable mentioned
// only once in a file, if it's a parameter of a method in an interface
// definition for instance.

function analysis2() {
  //ERROR: even if $ok was mentionned before, it's not ok anymore
  $ok = 1;
}

interface TestUnusedParameterOkInInterface {
  // this is ok, $p is not an unused parameter
  function analysis2bis($p);
}

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

