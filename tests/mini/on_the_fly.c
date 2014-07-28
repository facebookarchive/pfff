
// do function pointer that when called is passed the address of a certain
// global and so add info in point-to-set of this global pointer.

int global*;

void indirect(int* x) {
  *x = 42;
}

void (*pt)(int *x);


void main() {
  global = malloc(sizeof(int));
  pt = &indirect;
  int* local = global;
  (*pt)(local);
}
