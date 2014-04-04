<?php

// test data for some adhoc query, but prolog can solve this issue
class CF_InitAndRead {
  private $fld;

  public function __construct($x) {
    $this->fld = $x;
  }

  public function test1() {
    return $this->fld;
  }
}

class CF_Write {
  private $fld2;

  public function __construct($x) {
    $this->fld2 = $x;
  }

  public function test1() {
    return $this->fld2 = 3;
  }
}
