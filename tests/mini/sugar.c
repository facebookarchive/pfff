// hidden sugar
#include <stdlib.h>
#include <stdio.h>

static int afunc(int* x) {
}

static int (*pt)(int *x);

int main_sugar() {
  // you don't need &
  pt = afunc;
  int v = (*pt)(local);
  return 1;
}
