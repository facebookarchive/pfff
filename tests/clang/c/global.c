int global = 1;

void func_misc1() {
}

void use_global(int param) {
  int local;
  local++;
  param++;
  global++;
  void (*p)() = &func_misc1;
  
}
