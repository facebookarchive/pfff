<?php

class UseNamespace extends N\AInN 
{
}

use N\AInN;
function test_namespace_use() {
  $o = new AInN();
}

use N\AInN as Bar;
function test_namespace_alias() {
  $o = new Bar();
}

use \N\AInN;
function test_namespace_absolute() {
  $o = new AInN();
}

function test_namespace_global() {
  $o = new GlobalClass();
}