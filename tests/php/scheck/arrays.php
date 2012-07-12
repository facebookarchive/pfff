<?php

function test_undeclared_array($array) {
  $array['fld'] = 1;

  $arrayy['fld2'] = 1;

  echo $array;
  echo $arrayy;
}