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