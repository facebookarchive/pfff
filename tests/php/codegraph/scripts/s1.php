<?php

function foo() {
}

function main() {
  foo();

}

class DupeScript {
  public function foo() {
    foo();
  }
}
