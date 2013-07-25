<?php

$o = <x:misc></x:misc>;
$o = <x:misc
    xstr="hello"
    />;
$o = <x:misc
    //ERROR: undefined attribute
    xnosuchstr="world"
    />;
$o = <x:misc
    //ERROR: same error, undefined attribute
    xnosuchstr="world"
    />;
$o = <x:misc
    superstr="hello1"
    />;
$o = <x:misc
    x_str="test"
    />;
$o = <x:misc
    data-x="1"
    aria-x="2"
    srvr-x="3">
    </x:misc>;
function test_err() {
     $o = <x:require-field
     req_str="1"
     />;
     $o = <x:require-field
     req_str="1"
     req_int={1}
     />;
}
function test_no_err() {
    $o = <x:require-field
    req_str="1"
    />;
    $o->setAttribute('req_int', 0);
}