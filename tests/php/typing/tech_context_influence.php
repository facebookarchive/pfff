<?php

function myid($x) { 
  return $x; 
}

function context_influence() {
  $x = myid(1);
  return $x;
}

function context_influence2() {
  $x = myid(true);
  return $x;
}

