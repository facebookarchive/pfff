<?php

trait UseParentButNotParent {
  public function foo() {
    // generate a lookup failure right now
    parent::foo();
  }
}

