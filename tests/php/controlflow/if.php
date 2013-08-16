<?php
function foo() {
  $x = 0;
  if ($x < 1) {
    echo "less than 1";
  }
  else {
    $x = 1;
  }
  return $x;
}