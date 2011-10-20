<?php

function test1() {
  return foo('a                                                            ');
}

// we should keep unrelated code unchanged, even if badly indented
function not_related() {
  bar(
    1
  );
}

function test2() {
  return foo(1);
}
