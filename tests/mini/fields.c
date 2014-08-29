// aliasing, dataflow
#include <stdlib.h>
#include <stdio.h>

struct FooFields {
  // Where field can be assigned?
  int field;
  int field2;
};

static struct FooFields *foo;

void set_int(int *dst, int value) {
  // Foo.field can be assigned here, because dst can be alias for it
  // because it's aliased in main()
  *dst = value;
}

int main_fields() {
  int v = 42;
  foo = malloc(sizeof(struct FooFields));
  int *v2 = &foo->field;
  set_int(v2, v);
  int v3 = foo->field;
  printf("%d\n", v3);
  int v4 = 1;
  return v4;
}

