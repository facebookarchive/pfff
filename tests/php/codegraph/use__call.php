<?php

class DynMethods {
  public function __call() {
  }

  public function test__call() {
    return $this->dynCallVia__call();
  }
}