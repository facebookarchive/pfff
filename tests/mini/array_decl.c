
// this creates a new abstract memory location, just like malloc!
int* x[10];
int* y[10][20];

void main_array_decl() {
  x[0] = malloc(sizeof(int));
  x[0] = 42;
}
