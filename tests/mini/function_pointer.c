// callbacks
#include <stdlib.h>
#include <stdio.h>

static int global;

// Where this function can be called?
int proc_error() {
  global = 42;
  int v = 1;
  return v;
}

int (*error)();


static void foo() {
  // proc_error can be called here, because of main() assignment
  int x = (*error)();
}

int main_function_pointer() {
  error = &proc_error;
  foo();
  int v = global;
  printf("%d\n", global);
}

