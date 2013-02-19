int global = 1;

void use_global(int param) {
  int local;
  local++;
  param++;
  global++;
}
