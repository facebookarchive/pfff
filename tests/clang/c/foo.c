int bar(int a);

int foo(int a) {
  return 2*bar(a);
}


int bar(int a) {
  return 4*a;
}
