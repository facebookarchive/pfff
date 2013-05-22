<?php

// Many builtins are now transformed in regular function calls in
// Ast_php_simple. Those functions must then be defined in 
// data/php_stdlib/pfff.php otherwise you can get some
// 'Undefined function' errors.
// coupling: see the comment for builtin() in ast_php_simple.ml

function test_builtins() {
  //ERROR: undefined function
  undefined_function(1);

  //ERROR: please avoid dynamic function
  eval(1);
  $x = 'foo';
  //ERROR: please avoid dynamic code, eval_var()
  $$x = 1;
  $o = new TestDynField();
  $fld = 'afield';

  $o->$fld = 1;

  clone 1;

  exit(1);
  yield 1;
  yield break;
  unset($x);
  isset($x);
  empty($x);

  echo 'foo';
  print('foo');
  @print('foo');
  `ls`;

  include 'common.php';

  echo __FILE__;
}

class TestDynField {
}
