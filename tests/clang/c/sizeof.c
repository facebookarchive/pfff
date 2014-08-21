
struct Foo {
  int x;
};

void test_sizeof() {
  int y = sizeof(struct Foo);
  int z = sizeof y;
}
