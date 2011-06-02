<?php

// this is since php 5.3, 
// see http://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.nowdoc

function foo() {

  $x = 1;
//TODO: handle nowdoc!
// note that strings are _not_ interpolated inside nowdoc
  return <<<'ENDGETTER'
    this is $x
ENDGETTER;

}

//   this is $x
echo foo();
