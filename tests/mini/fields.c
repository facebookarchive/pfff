// aliasing, dataflow
#include <stdlib.h>
#include <stdio.h>

struct Foo {
  int field;
  int field2;
};

struct Foo *foo;

void set_int(int *dst, int value) {
  *dst = value;
}

int main() {
  int v = 42;
  foo = malloc(sizeof(struct Foo));
  set_int(&foo->field, v);
  int v2 = foo->field;
  printf("%d\n", v2);
  int v3 = 1;
  return v3;
}



