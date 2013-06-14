<?php

function bar() {
}

function main() {
  bar();
}

//ERROR: bad style, duplicated class
class DupeScript {
  public function foo() {
    bar();
  }
}
