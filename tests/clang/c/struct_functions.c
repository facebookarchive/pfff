
struct foo_obj {
  void (*open)();
};

extern struct foo_obj x;

static void foo_open() {
}

struct foo_obj aclass_with_methods = {
  foo_open
};
