<?php

trait UseParentButNotParent {
  final public function __construct() {
    // generate a lookup failure right now
    //SKIP: parent::__construct();
  }
}

