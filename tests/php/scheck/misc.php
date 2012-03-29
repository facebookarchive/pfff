<?php

function test_case() {
  switch(2) {
    case 1:
      return 1;
    //ERROR: please use ':', not ';'
    case 2;
      return 2;
    default:
      return 42;
      
  }
}

class MiscA { }
class MiscB { }

function test_instanceof() {
  $x = new MiscA();
  if ($x instanceof MiscA) {
  }
  //ERROR: please use 'instanceof'
  if ($x instanceOf MiscA) {
  }
  
}

var_dump(test_case());
