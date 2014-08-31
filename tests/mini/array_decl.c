
// this creates a new abstract memory location, just like malloc!
int* x[10];
int* y[10][20];

int** z;

void main_array_decl() {
  x[0] = malloc(sizeof(int));
  *(x[0]) = 42;

  // I used to have a point_to/2 and array_point_to/2, but this can not
  // work because of such code. Array and pointers are very equivalent
  // and you can't use different predicates.
  z = x;
  *(z[0]) = 43;
}
