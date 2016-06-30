<?php

//Declare variables so that "Undeclared variable" check isn't triggered.
$a = $b = $c = $d = $e = $f = $g = $h = $i = $j = $k = $l = $m = $n = $o = $p = $q = $r = $s = $t = $u = $x = $y = $z = true;

//ERROR: duplicate variable $a
if ($a || $b || $a) {}

//ERROR: duplicate variable $a
if ($a and $a) {}

//ERROR: duplicate variable $a
if ($a or $a) {}

//No error on $a due to && with higher precedence
if ($a and $b && $a) {}

//ERROR: duplicate variable $c
if (($a and $b && $a && ($c & ($c | $c)))) {}

//ERROR: duplicate variable $c
if (($a and $b && $a && ($c & ($c & $c)))) {}

//ERROR: duplicate variable $m
if ($a && $b || $c || $m && $m) {}

// No error on $m due to operator precedence
if ($a || $b || $c && $m || $m) {}

//ERROR: duplicate variable $b (checks with parentheses)
if (($a || (($b)) || ((($b)))) || ($b)) {}

//No error on $x due to operator precedence
if ($a || $b || $c && $d || $e && $f || $x || $y && $g && $h && $i || $d || $j && $k && $l && $x && $z) {}

//ERROR: duplicate variable $e
if ($a || $b || $c || $d || $e && $e && ($f && ($g || $z) && $h && $i)) {}

//ERROR: duplicate variable $o
if ($i || $j || $k || ($l || $m) && $n && $o && ($p || $q) && $r && ($s && ($t && ($o && $u)))) {}
?>
