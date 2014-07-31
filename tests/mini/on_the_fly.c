
// do function pointer that when called is passed the address of a certain
// global and so add info in point-to-set of this global pointer.

int* global;

int indirect(int* x) {
  int v = 8;
  *x = v;
  return v;
}

int (*pt)(int *x);


void main() {
  global = malloc(sizeof(int));
  pt = &indirect;
  int* local = global;
  int v = (*pt)(local);
}
