<?php

function test_undeclared_array($array_parameter) {
  $array_parameter['fld'] = 1;

  //ERROR: use of undefined array (this used to be ok in the old var checker)
  $undefined_array['fld2'] = 1;

  echo $array_parameter;
  //ERROR: use of undefined array
  echo $undefined_array;
  
  // What about $array[] = 1; Is it more ok? but people can just
  // do $array = array(1); and if it's inside a foreach they really should
  // declare the array before anyway.
}

