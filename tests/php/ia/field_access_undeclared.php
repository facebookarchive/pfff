<?php

class A {
  public $field;
  //public $fld;
}
$o = new A();
// this is not ok in strict mode
$o->fld = 1;
var_dump($o);
