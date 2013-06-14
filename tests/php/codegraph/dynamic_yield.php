<?php

trait DynamicYield {
  public function __call() {
    // supposed to do some magic to transform calls to prepareXxx to yieldXxx
  }
}

class TestDynamicYield {
  public function yieldXxx() {
  }

  public function testDynamicYield() {
    return $this->prepareXxx();
    //TODO: but this error should be detected: 
    // $this->prepareYyyy();
  }
}

