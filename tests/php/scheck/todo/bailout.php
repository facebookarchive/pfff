<?php

function foo_bailout() {
  $a = "foo";
  $foo = 2;

  echo $$a;
}

$glob = 1;

//function foo2_bailout() {
//  $s = "glob";
//  global $$s;
//
//  echo $$s;
//}

