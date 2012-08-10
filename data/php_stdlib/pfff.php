<?php

// Some of the unsugaring done in ast_simple_build.ml transforms
// certain builtins like echo in regular function calls.

function __builtin__echo($xs) { 
}

// we don't process include/require so the body of this function is empty
function __builtin__require($f) { }
function __builtin__require_once($f) { }
function __builtin__include($f) { }
function __builtin__include_once($f) { }

function __builtin__isset($x) {
}
function __builtin__unset($x) {
}