
static int global;

static void static_function() {
}

void call_static_function() {
  static_function();
}

static void use_static_global() {
  global++;
}
