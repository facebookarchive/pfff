<?php

function test_format_string() {
  printf('Hello World from %s\n', 'Jiao');
  //ERROR: Number of arguments in printf does not match the format string
  printf("Hello World from %s %s\n", 'Jiao');
  //ERROR: Number of arguments in printf does not match the format string
  printf("Hello World from %s, %d times\n", 'Jiao');
  //ERROR: Number of arguments in printf does not match the format string
  printf("%s %d\n", "1", 2, "3");
  //ERROR: Number of arguments in printf does not match the format string
  printf();
  //ERROR: Number of arguments in invariant does not match the format string
  invariant(false, );
  //ERROR: Number of arguments in invariant does not match the format string
  invariant(false);
  //ERROR: Number of arguments in SQL does not match the format string
  SQL("Select %s From %s Where %s",
    "name",
    "Atable",
  );
  //ERROR: Number of arguments in SQL does not match the format string
  SQL("Select %s From %s Where %s",
    "name",
    "Atable"
  );
}
