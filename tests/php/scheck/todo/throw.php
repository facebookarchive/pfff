<?php

// this one is now done in cmf at least

//FATAL
throw Exception();

throw new Exception();

function test_throw() {
  try {
    throw Exception();
  } catch (Exception $exn) {
  }
}