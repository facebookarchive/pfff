<?php

// I have no idea what is the difference between using
// $o = new A(); and $o =& new A();

class A {
}

function test1() {
  $o = new A();
  var_dump($o);
  return $o;
}

$o = test1();
var_dump($o);

function &test2() {
  $o =& new A();
  var_dump($o);
  return $o;
}

$o2 = test2();
var_dump($o2);


