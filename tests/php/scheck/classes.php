<?php

class CA {
}

class CDUP {
}

class CDUP {
}


function classes_foo1() {
  $o = new CA();
  echo $o;

  //ERROR: not defined
  $o = new CUnknwown();

  //ERROR: multi defined
  $o = new CDUP();

  // we generate an error message just for the first one
  $o = new CDUP();
}

class CB extends CA {
}


//ERROR: undefined class
class CC extends CUnknwown {
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

    //ERROR: undefined member (private)
    $this->member0=0;
  }
}
