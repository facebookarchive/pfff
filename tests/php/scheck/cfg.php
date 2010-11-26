<?php

function foo() {
  
  $a = 1;
  echo $a;
  return 1;
  //ERROR: dead statement
  echo $a;
}