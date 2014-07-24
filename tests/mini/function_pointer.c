// callbacks
#include <stdlib.h>
#include <stdio.h>

int global;

void proc_error() {
  global = 42;
}

void (*error)();


void foo() {
  (*error)();
}

int main() {
  error = &proc_error;
  foo();
  int v = global;
  printf("%d\n", global);
}




