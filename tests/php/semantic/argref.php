<?php

function foo($x) {
  $x = $x + 10;
}

$y = 5;
foo($y);
echo $y, "\n";


$y = 5;
foo(&$y);
echo $y, "\n";
