<?php

class :x:foo {
  attribute
    int x @required,
    float y,
    //SKIP: would be good to detect that
    UnknownClass z
    //var z // do not lookup for __var__
    ;
}

function test_required() {
    //ERROR: Undefined required xhp field x
    return <x:foo />;
}
