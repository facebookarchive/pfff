<?php

class Exception {
}

function var_dump(...) { }

function range($x, $y) { return $x + $y; }

function printf($s, ...) { return $s; }
function sscanf(...) { }

function invariant(...) { }

function array_count_values($arr) { return $arr; }

function func_num_args() { }
function call_user_func_array(...) { }
function func_get_args(...) { }

function SQL(...) { }

function dirname(...) { }

// mostly a copy of data/php_stdlib/pfff.php

function __builtin__echo($xs/*...*/) { }
function __builtin__isset($x) { }
function __builtin__clone($expr) { }
function __builtin__exit($exit_code) { }
function __builtin__yield($expr) { }
function __builtin__print($expr) { }
function __builtin__at($expr) { }
function __builtin__include($f) { }
function __builtin__exec($cmd) { }
