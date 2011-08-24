
class Foo {
  ~Foo() { }
  Foo(const Foo&) { }
  Foo& operator=(const Foo&) { }
};

class Bar {
  ~Bar() { }
  Bar(const Foo&) { }
  //Bar& operator=(const Bar&) { }
};
