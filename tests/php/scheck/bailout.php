<?php

function foo() {
  $a = "foo";
  $foo = 2;

  echo $$a;
}

$glob = 1;

function foo2() {
  $s = "glob";
  global $$s;

  echo $$s;
}