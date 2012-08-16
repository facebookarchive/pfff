<?php

function test_undeclared_array($array_parameter) {
  $array_parameter['fld'] = 1;

  //ERROR: use of undefined array
  $undefined_array['fld2'] = 1;

  echo $array_parameter;
  //ERROR: use of undefined array
  echo $undefined_array;
}

