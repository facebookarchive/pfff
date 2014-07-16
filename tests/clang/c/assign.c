
int my_global;

void test_incr_global() {
  my_global = my_global + 1;
}

void test_incr_global2() {
  my_global += 1;
}

void test_incr_global3() {
  my_global++;
}

int rvalue_global() {
  return my_global;
}

void lvalue_global() {
  my_global = 1;
}

void modify_via_ref(int *ref) {
  *ref = 1;
}

void via_pointer() {
  modify_via_ref(&my_global);
}

