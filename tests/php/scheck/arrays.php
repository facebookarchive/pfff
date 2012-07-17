<?php

function test_undeclared_array($array) {
  $array['fld'] = 1;

  //ERROR: use of undefined array
  $arrayy['fld2'] = 1;

  echo $array;
  //ERROR: use of undefined array
  echo $arrayy;
}