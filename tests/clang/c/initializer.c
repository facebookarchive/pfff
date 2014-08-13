
struct FooInitializer {
  int x;
  int y;
};


struct FooInitializer globalInitializer = {
  // the lines are actually reordered in the .clang, and the field
  // name is not there (unsugaring), so we need to do additional work
  // in graph_code_clang to handle such dependencies
 .y = 2,
 .x = 1,
};

void test_use_field_directly() {
  globalInitializer.x = 1;
  globalInitializer.y = 1;
}
