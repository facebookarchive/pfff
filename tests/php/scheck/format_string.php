<?php

function test() {
  printf('Hello World from %s\n', 'Jiao');
  printf("Hello World from %s %s\n", 'Jiao');
  printf("Hello World from %s, %d times\n", 'Jiao');
  printf("%s %d\n", "1", 2, "3");
  printf();
  invariant(false);
}
