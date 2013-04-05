<?php

class :x:foo {
  attribute
    int x,
    float y,
    //SKIP: would be good to detect that
    UnknownClass z
    //var z // do not lookup for __var__
    ;
  

}