<?php

$x = 42;
//$x:&2{&1{42}}
var_dump($x);
$x = 2;
//$x:&2{&1{int}}, no range, go very abstract very quickly
var_dump($x);
//$y will be a copy of $x (no sharing)
$y = $x;
var_dump($y);

