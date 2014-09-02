
struct StructArray {
  int x;
  // hidden malloc here when malloc(sizeof(StructArray))
  int* y[10];
};


static struct StructArray global;

void test_struct_array() {
  global.y[0] = malloc(sizeof(int));

  // this works with this, but the array decl in the struct
  // should not force people to have to do this!
  //global.y = malloc(4 * sizeof(int*));

  *(global.y[0]) = 42;
}

