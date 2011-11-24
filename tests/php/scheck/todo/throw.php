<?php

//FATAL
throw Exception();

throw new Exception();

function test_throw() {
  try {
    throw Exception();
  } catch (Exception $exn) {
  }
}