<?php


function string_use() {
  $a = "<html> is bad in string </html>";
  $xhp = <this-is-good>$a</this-is-good>;
  $url = $_SERVER['PHPROOT'] . "/path/to/url.php";
}

function dynamic_calls($b) {
  a_foo();
  $a = 'a_foo';
  $a();

  $b();
  call_user_func($a, array());
}

function call_a_dynamic_caller() {
  $b = 'b_foo';
  a_foo();
  dynamic_calls($b);
}

function take_arg2_by_ref($a, &$argref, $c) {
}

function byref_calls() {
  $a = 1;
  $b = 2;
  $c = 3;
  take_arg2_by_ref($a, $b, $c);
}