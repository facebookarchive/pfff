<?php


function setLimit(int $limit) {
}

function setLimit2(int $limit) mixed {
  int $i = 1;
  $a = 'string';   // this should throw becuase it was undeclared.
}


class A {
  private string $description;
  private int $nObjects;
}
