<?php

$o = <x:misc></x:misc>;
$o = <x:misc
    xstr="hello"
    />;
$o = <x:misc
    //ERROR: undefined attribute
    xnosuchstr="world"
    />;