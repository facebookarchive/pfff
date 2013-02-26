<?php

class ProtectedUsed {
  // default is public in PHP
  static $public_static;

  protected $fld;
  protected $dead_fld;
  function method1() {
    $this->fld = 1;
  }
}

class ProtectedUsedChild extends ProtectedUsed {
  function method2() {
    $this->fld = 1;
  }
}


class ProtectedNotThatUsed { 
  protected $fld;
  function method1() {
    $this->fld = 1;
  }
}

