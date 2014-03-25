int global = 1;

int global2;

void use_global(int param) {
  int local;
  extern int global2;
  local++;
  param++;
  global++;
  global2++;
}
