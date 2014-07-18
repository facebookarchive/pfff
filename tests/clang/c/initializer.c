
struct FooInitializer {
int x;
int y;
};


// TODO: this is not currently handled :(
struct FooInitializer globalInitializer = {
  // the lines are actually reordered in the .clang, normalization.
 .y = 2,
 .x = 1,
};

void test_use_field_directly() {
  globalInitializer.x = 1;
  globalInitializer.y = 1;
}
