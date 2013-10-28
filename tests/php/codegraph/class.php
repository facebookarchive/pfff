<?php

class A {
  public $fld;
  static $static_var;
  const A_CST = 1;


  public function test_use_field() {
    return $this->fld;
  }
  public function test_use_undefined_field() {
    //ERROR: use of undefined field (bug skipped error for now)
    return $this->undefined_fld;
  }

  public function test_static_var() {
    return self::$static_var;
  }

  public function test_undefined_static_var() {
    //ERROR: undefined static field
    return self::$undefined_static_var;
  }

  public function test_constant() {
    return self::A_CST;
  }

  public function test_undefined_constant() {
    //ERROR: undefined class constant
    return self::UNDEFINED_CLASS_CONSTANT;
  }

  public function test_field_function() {
    ($this->fld)();
  }
}

function use_A() {
  $o = new A();
  return $o;
}

class TestUndefinedClass extends UndefinedExtendedClass {
}
function use_undefined_class() {
  //ERROR:
  //$o = new UndefinedClass();
  $x = UndefinedClass::UNDEF_CST_IN_UNDEFINED_CLASS;
}
