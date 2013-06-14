<?php

// see Graph_code_analysis.protected_to_private()

class ProtectedUsed {
  // default is public in PHP
  static $public_static;

  protected $fld;
  protected $dead_fld;
  protected $redefined_in_child;
  function method1() {
    $this->fld = 1;
  }
}

class ProtectedUsedChild extends ProtectedUsed {
  protected $redefined_in_child;

  function method2() {
    $this->fld = 1;
    $this->redefined_in_child = 1;
  }
}


class ProtectedNotThatUsed { 
  protected $fld;
  function method1() {
    $this->fld = 1;
  }
}

