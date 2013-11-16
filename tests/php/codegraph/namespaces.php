<?php

namespace N;
class AInN {
}

function f1_in_N() {
  f2_in_N();
}

function f2_in_N() {
  echo 'f2_in_N\n';
}

f1_in_N();

namespace {
  class GlobalClass {
  }
}