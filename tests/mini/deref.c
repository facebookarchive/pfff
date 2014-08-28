
int main_deref() {
  int x;
  int* y;
  int **w;
  int *z;
  x = 2;
  y = &x;
  w = &y;
  z = *w;
  return x;
}
