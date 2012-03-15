<?php

class Foo {
  static function myid($x) { 
    return $x; 
  }
}

function context_influence() {
  $x = Foo::myid(1);
  return $x;
}
  
function context_influence2() {
  $x = Foo::myid(true);
  return $x;
}

