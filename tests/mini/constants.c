// information flow
#include <stdlib.h>
#include <stdio.h>

struct Foo {
 int state1;
 int state2;
};

// enum1 should flow in Foo.state1
enum {
  ENUM1 = 42,
  ENUM2 = 43,
};

struct Foo *global;

void bar(int param) {
  global->state1 = param;
}

void foo(int param) {
  int local = param;
  bar(local);
}

int main() {
  global = malloc(sizeof(struct Foo));
  foo(ENUM1);
  int v = global->state1;
  printf("%d\n", v);
}

