<?php

function bar() {
}

function main() {
  bar();
}

class DupeScript {
  public function foo() {
    bar();
  }
}
