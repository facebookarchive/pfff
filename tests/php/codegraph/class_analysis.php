<?php

class Complex {
  public function unique() { }
  public function ambiguous() { }
}

class Complex2 {
  public function ambiguous() { }
}


function test_class_analysis($o) {
  //TODO: this should be ok
  //$o->unique();
  $o->ambiguous();
  $o->unknown_method();
}