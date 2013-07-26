<?php

class AnotherClassLikeThis {
}

function test1() {
  $o = new AnotherClassLikeThis();
  $o = new anotherclasslikeThis();
}


class DynB {
}

function test_dynamic_class_bis() {
  echo dynb::$fld;
}