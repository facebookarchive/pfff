
struct foo_obj {
  void (*open)();
};

static void foo_open() {
}

struct foo_obj x = {
  foo_open
};
