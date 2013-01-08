<?php

trait UseParentButNotParent {
  final public function __construct() {
    parent::__construct();
  }
}

