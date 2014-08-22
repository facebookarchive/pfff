// combination array, field, call!
#include <stdlib.h>
#include <stdio.h>

struct Foo {
  int (*fld)();
};

struct Foo* vctls;

// Where this interrupt function can be called?
int i850intr() {
  printf("in i850intr\n");
  int v = 1;
  return v;
}

int intrenable(int idx, int (*f)()) {
  struct Foo* v;
  v = &vctls[idx];
  v->fld = f;
  int v2 = 1;
  return v2;
}



int trap() {
  int i = 1;
  while(i) {
    struct Foo* v;
    int i2 = 0;
    v = &vctls[i2];
    while (i) {
      // i850intr can be called here because it flows in intrenable,
      // which flows in foo.fld and foo.fld is used here
      int (*callback)();
      callback = v->fld;
      int res;
      res = (*callback)();
      i = 0;
    }
  }
  int v3 = 1;
  return v3;
}


int main() {
  int (*f)();
  f = &i850intr;
  int size = 10;
  vctls = malloc(size * sizeof(struct Foo));
  int v0 = 0;
  int v1 = intrenable(v0, f);
  int v2 = 1;

  trap();
  return v2;
}

void printf(char* fmt) {
}
void* malloc(int x) {
}
