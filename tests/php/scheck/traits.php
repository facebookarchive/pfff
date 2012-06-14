<?php

trait T {
  public static function foo() {
  }
}
class AUseT {
  use T;
}

class TestLookupMethodTrait extends AUseT  {
}

// This is ok
TestLookupMethodTrait::foo();

trait TraitCallParent {
  public function testParent() {
    // this is ugly, but ok. If there is an error it should be 
    // checked at 'use time'.
    parent::init();
  }
}

class TestTraitCallParent {
  //SKIP: should say that this class should extend something with an init().
  use TraitCallParent;
}
class HasInit {
  static public function init() { }
}

class TestTraitCallParent2 extends HasInit {
  // this is ok
  use TraitCallParent;
}

