// information flow
#include <stdlib.h>
#include <stdio.h>

struct Foo {
 int state1;
 int state2;
};

// enum1 should flow in Foo.state1
int enum1;
int enum2;

struct Foo *global;

void bar(int param) {
  global->state1 = param;
}

void foo(int param) {
  int local = param;
  bar(local);
}

int main() {
  enum1 = 42;
  enum2 = 43;
  global = malloc(sizeof(struct Foo));
  foo(enum1);
  int v = global->state1;
  printf("%d\n", v);
}

