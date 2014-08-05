<?php

function foo($x) {
  return $x + 1;
}

//ERROR: typing error
foo(array("bar"));
