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
    aria-x="2">
    </x:misc>;
