
struct FooInitializer {
int x;
int y;
};


// TODO: this is not currently handled :(
struct FooInitializer globalInitializer = {
 .x = 1,
 .y = 2,
};

void test_use_field_directly() {
  globalInitializer.x = 1;
  globalInitializer.y = 1;
}
