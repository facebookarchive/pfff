<?php

function foo($x) {
  $x = $x + 10;
}

$y = 5;
foo($y);
echo $y, "\n";


$y = 5;
//ERROR: bad, passing reference where was expecting regular parameter
foo(&$y);
echo $y, "\n";


function taking_a_ref(&$x) {
  $x = $x + 10;
}

$y = 5;
taking_a_ref(&$y);
//SKIP: not indicating a "pass-by-ref" at call site is bad, but skip cos too many
taking_a_ref($y);

//ERROR: keyword argument to reference parameter is bad
taking_a_ref($x = 2);

