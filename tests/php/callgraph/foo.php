<?php

function f() {
  f2();
}

function f2() {
  f3();
}

function f3() {
  f4();
  f5();
}

function f4() {
  f6();
}

function f5() {
  f3();
}

function f6() {
}
