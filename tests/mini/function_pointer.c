// callbacks
#include <stdlib.h>
#include <stdio.h>

int global;

// Where this function can be called?
void proc_error() {
  global = 42;
}

void (*error)();


void foo() {
  // proc_error can be called here, because of main() assignment
  (*error)();
}

int main() {
  error = &proc_error;
  foo();
  int v = global;
  printf("%d\n", global);
}

