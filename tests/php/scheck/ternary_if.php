<?php
function test () {
  $l = 1;
  $r = -1;
  //ERROR: ternary if ("?:") is not necessary here, use the condition or its negation.
  $o = true ? TRUE : False;
  //ERROR: ternary if ("?:") is not necessary here, use the condition or its negation.
  $o = ($l == $r) ? true : false;
  $o = ($l = $r) ? true : false;
  //ERROR: ternary if ("?:") is not necessary here, use the condition or its negation.
  $o = ($l < $r) ? true : false;
  $o = ($l < $r) ? $l : $r;
  //ERROR: ternary if ("?:") is not necessary here, use the condition or its negation.
  $o = $l < $r ? true : false;
  //ERROR: ternary if ("?:") is not necessary here, use the condition or its negation.
  $o = ($l && $r) ?: false;
}