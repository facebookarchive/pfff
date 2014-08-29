
int global;

static int* foo(int *param) {
  int *local = param;
  return local;
}

int main_function() {
  int *pt;
  pt = &global;
  int *pt2;
  // pt2 can be an alias for global
  pt2 = foo(pt);
}
