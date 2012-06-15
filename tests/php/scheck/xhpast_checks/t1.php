<?php

//function () use ($c) {
//  $c++;
//};

function f($a, $b) {
  static $c, $d;
  global $e, $f;
  $g = $h = x();
  list($i, list($j, $k)) = y();
  foreach (q() as $l => $m) {
    
  }

  $a++;
  $b++;
  $c++;
  $d++;
  $e++;
  $f++;
  $g++;
  $h++;
  $i++;
  $j++;
  $k++;
  $l++;
  $m++;
  $this++;
  $n++; // Only one that isn't declared.
  
  extract(z());
  
  $o++;
}

function g($q) {
//  $$q = x();

  $r = y();
}

class C {
  public function m() {
    $a++;
    x($b);
    $c[] = 3;
    $d->v = 4;
    $a = $f;
  }
}

function worst() {
//  global $$x;
  $y++;
}

function superglobals() {
  $GLOBALS[$_FILES[$_POST[$this]]]++;
}

function ref_foreach($x) {
  foreach ($x as &$z) {
    
  }
  $z++;
}

function has_default($x = 0) {
  $x++;
}

function declparse(
  $a,
  Q $b,
  Q &$c,
  Q $d = null,
  Q &$e = null,
  $f,
  $g = null,
  &$h,
  &$i = null) {
  $a++;
  $b++;
  $c++;
  $d++;
  $e++;
  $f++;
  $g++;
  $h++;
  $i++;
  $j++;
}

function declparse_a(Q $a) { $a++; }
function declparse_b(Q &$a) { $a++; }
function declparse_c(Q $a = null) { $a++; }
function declparse_d(Q &$a = null) { $a++; }
function declparse_e($a) { $a++; }
function declparse_f(&$a) { $a++; }
function declparse_g($a = null) { $a++; }
function declparse_h(&$a = null) { $a++; }

function static_class() {
  SomeClass::$x;
}

function instance_class() {
  $a = $this->$x;
}

function exception_vars() {
  try {
  } catch (Exception $y) {
    $y++;
  }
}

function nonuse() {
  isset($x);
  empty($y);
  $x++;
}

function twice() {
  $y++;
  $y++;
}

function more_exceptions() {
  try {
  } catch (Exception $a) {
    $a++;
  } catch (Exception $b) {
    $b++;
  }
}
    
class P {
  abstract public function q();
}
