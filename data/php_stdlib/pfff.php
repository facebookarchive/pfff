<?php

// Some of the unsugaring done in ast_simple_build.ml transforms
// certain builtins like 'echo' in regular function calls so those
// builtins must now be defined somewhere.

function __builtin__echo($xs/*...*/) { }
function __builtin__print($expr) { }
function __builtin__at($expr) { }

// we don't process include/require so the body of this function is empty
function __builtin__require($f) { }
function __builtin__require_once($f) { }
function __builtin__include($f) { }
function __builtin__include_once($f) { }

function __builtin__isset($x) { }
function __builtin__unset($x/*...*/) { }
function __builtin__empty($x) { }

function __builtin__eval($expr) { }
function __builtin__eval_var($fld) { }
function __builtin__eval_var_field($fld) { }

function __builtin__clone($expr) { }
function __builtin__exit($exit_code) { }
function __builtin__yield($expr) { }
function __builtin__yield_break($expr) { }

function __builtin__exec($cmd) { }

