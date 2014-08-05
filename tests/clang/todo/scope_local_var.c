
int x;

void f() {
  {
    int x;
  }
  // this should be the ref to the global
  return x;
}
