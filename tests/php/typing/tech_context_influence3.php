<?php

class Foo {
  // Here, 
  static function myid($x) { 
    return $x; 
  }

  static function context_influence() {
    $x = Foo::myid(1);
    return $x;
  }
  
  static function context_influence2() {
    $x = Foo::myid(true);
    return $x;
  }
}



