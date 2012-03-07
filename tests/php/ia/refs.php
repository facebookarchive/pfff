<?php

function main() {
  $x = 2;
  var_dump($x);
  $y =& $x;
//  $y = 3;
  var_dump($x);
  var_dump($y);
}

main();
