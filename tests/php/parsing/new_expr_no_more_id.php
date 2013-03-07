<?php

class NoId {
  public function foo() {
    echo "foo\n";
  }
  public function bar() {
    echo "bar\n";
  }
}

function id($x) {
  return $x;
}

$o = (new NoID)->foo()->bar();
$o = (new NoID())->foo()->bar();

// old way
$o = id(new NoID)->foo()->bar();


