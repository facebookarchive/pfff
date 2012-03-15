<?php

function foo($x) {
  $x = 1;
  $x = true;
  // int > bool
  return $x;
}
