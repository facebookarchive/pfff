<?php

function fncall($a) {
  return $a ? false : true;
}

function test_lint() {
  $a = 0;

  if (($a = 1)) {
    //ERROR: use == or add another set of parens around the assignment
    switch ($a = 2) {
      default:
        do {
          fncall(false);
        } while (($a = 1));
        if ($a == 1) {
        }
        return 42;
    }
  }

  //ERROR: use == or add another set of parens around the assignment
  if ($a = 5) {
    switch(($a = 3)) {
      default:
        do {
          fncall(false);
        //ERROR: use == or add another set of parens around the assignment
        } while ($a = 1);
        return 43;
    }
  }

  // make sure this one does NOT trigger the error
  if (fncall($a = true)) {
    while (($a = 5)) {
      //ERROR: use == or add another set of parens around the assignment
      while ($a = 6) {
        //ERROR: use == or add another set of parens around the assignment
        for ($a = 0; $a = 1; $a++) {
          for ($a = 0; ($a = 1); $a++) {
          }
        }
      }
    }
  }
}

var_dump(test_lint());
