// information flow
#include <stdlib.h>
#include <stdio.h>

struct Foo {
 int state1;
 int state2;
};

// Where ENUM1 can flow? ENUM1 can flow in Foo.state1
// because it's passed in foo(), then assigned in local, then
// passed in bar() and finally assigned in global->state1
enum {
  ENUM1 = 42,
  ENUM2 = 43,
};

static struct Foo *global;

void bar(int param) {
  global->state1 = param;
}

static void foo(int param) {
  int local = param;
  bar(local);
}

int main_constants() {
  global = malloc(sizeof(struct Foo));
  int v1 = ENUM1;
  foo(v1);
  int v2 = global->state1;
  printf("%d\n", v2);
}
