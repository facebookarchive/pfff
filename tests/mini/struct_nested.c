
struct NestedStruct {
  int w;
}

struct StructNested {
  int x;
  // hidden malloc here when malloc(sizeof(StructArray))
  int* y[10];

  // todo? matters? once we will not be just field-based, maybe
  // it will matters
  struct NestedStruct z;
};

static struct StructNested global;
