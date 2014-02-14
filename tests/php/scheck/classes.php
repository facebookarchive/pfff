<?php

class CA {
  const CstA = 0;
  public static $vA;
}

class CB extends CA {
}

class ClassDupe {
}

//SKIP: should return an error there too.
class ClassDupe {
}

// TODO: check with new self, new parent, etc

function test_new_unknown_class() {
  $o = new CA();
  echo $o;

  //ERROR: not defined
  $o = new CUnknwown();

  //ERROR: multi defined
  $o = new ClassDupe();

  // we generate an error message just for the first one
  $o = new ClassDupe();
}

function test_unknown_class_constant() {
  echo CA::CstA;

  //ERROR: not defined
  echo CA::UnknownConstant;

  // there is actually a lookup even on class constant ... ugly
  echo CB::CstA;

  //ERROR: not defined
  echo CB::UnknownConstant;

  // this is ok
  echo CB::class;
}

function test_unknown_class_variable() {
  echo CA::$vA;

  //ERROR: not defined
  echo CA::$unknown;

  // there is actually a lookup even on static class variable ... ugly
  echo CB::$vA;

  //ERROR: not defined
  echo CB::$unknown;

}

//ERROR: undefined class
class test_extend_unknown_class extends CUnknwown {
}


class TestUndefinedMembers {
  private $member0;
  protected $member1;
  public function testMembers() {
    $this->member0=0;
    $this->member1=0;

    //ERROR: use of undefined member
    $this->member2=0;
  }
}

class TestUndefinedMembers2 extends TestUndefinedMembers {
  private $member2;
  public function testMembers() {
    $this->member1=0;
    $this->member2=0;
    //ERROR: undefined member
    $this->member3=0;

    //SKIP: undefined member (private)
    $this->member0=0;
  }
}

class TestUndefinedMembers3 extends TestUndefinedMembers2 {
  public function test_three_levels() {
    echo $this->member1;
    
    //ERROR:
    echo $this->member_unknown;
  }
}
