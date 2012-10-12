class NestedClass {

  class nestedA {
    // no need to qualify with NestedClass.nestedB
    nestedB x;
  }

  class nestedB {
    int y;
  }
}
