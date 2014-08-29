<?php

//*************************************************************************
// Use of undefined variables and unused variables
//*************************************************************************
 
function test_undefined_and_unused_variables_basic(
  $param, 
  //ERROR: unused parameter
  $unused_parameter) {

  $ok = 1;
  // misc1() is defined in common.php
  misc1($ok);
  misc1($param);

  //ERROR: unused variable
  $unused_var = 1;

  //ERROR: use of undefined variable
  echo $undefined;

  //such calls would be arguably ok if misc1() was taking its arguments
  //by reference (see also variables_fp.php).
  //ERROR: use of undefined variable
  misc1($undefined_bis);
}

function test_unused_unset() {
  //ERROR: unused variable. Yes it's used by unset but this should not count.
  $unset_variable = 1;
  unset($unset_variable);

  $arr = array();
  $arr['field'] = 1;
  unset($arr['field']);

  // you can also unset the field of an object, or even a class variable
  // but I am not sure why you want that ...
  unset($arr[0], $arr[1]);

  //ERROR: it's not ok to unset a field of an array not declared
  unset($arr2[0]);
}

function test_unused_foreach() {
  $matches = array();

  foreach($matches as $k) {
    echo $k;
  }

  //ERROR: unused variable
  foreach($matches as $unused_key_in_foreach) {
  }

  // $k2 is not used but it's ok, as long as one of the key/value is used
  foreach($matches as $unused_but_ok => $v) {
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
    //TODO: one should not need to echo $k, the assignment just before
    // should actually count as a use of $k.
    echo $k;
  }

  // you can also do things like foreach($arr as $arr2[0]) but it's wierd
}

function test_unused_list() {
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

  // this is ok too
  list($ok1, $ok2, list($ok3, $ok4)) = misc1(1);
  echo $ok1;
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

function test_automatic_closing_short_lambda() {
  $a = 42;
  // this is ok
  $f = () ==> $a;

  list($b, $c) = array();
  $f = () ==> $b;
  return $f;
}

function test_automatic_closing_short_lambda() {
  //ERROR: use of undefined
  $f42 = () ==> $a;
  return $f42;
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
  public function test_lambda_this_passed_implicitly($a) {
    $f = (function ($b) use($a) {
        // this is now ok in recent PHP, no need to pass $this in use()
        var_dump($this);
        return $a + $b;
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

$aglobal = 1;
//TODO: globals should have an access count initialized to 1, it's ok
// if they are not used in the current file as long as someone else
// somewhere uses it
echo $aglobal;
$another_global = 1;
echo $another_global;

function test_global() {
  global $aglobal;

  //SKIP: this requires a global analysis
  global $undefined_global;
  // ok
  echo $aglobal;
  //SKIP:
  echo $undefined_global;
}

class TestTypoSelf {
  const A_CST = 1;
  
  public function test_typo_self() {
    //ERROR: use of undeclared variable $self
    return $self::A_CST;
  }
}

function test_variadic($x, ...$args) {
  echo $x;
  echo $args;
}
//*************************************************************************
// Misc regressions
//*************************************************************************

class TestDynProp {
  public $fld;
}
// I was originally not generating the right Ast for dynamic prop access
function test_dynamic_field() {
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
