#include <stdlib.h>
#include <stdio.h>

int main_array () {
  int y = 10;
  int* x = malloc(y * sizeof(int));
  int w = x[y];

  int z = 2;
  x[y] = z;

  int *elt;
  elt = &x[y];
  int z2 = 4;
  *elt = z2;
  return z;
// => array_elt_in_main_line3 should point to 2 or 4
}
