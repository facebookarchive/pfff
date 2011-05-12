<?php

function test_undeclared_and_closure($a) {

  $c = "foo";

  $f = (function ($b) use($a) {
    return $a +
           $b +
           $c;
    ;
    });
  return $f;
}
