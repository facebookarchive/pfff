// where this syscall is called?
void sysnop() {
}

void (*)() systab[] = {
  [0] = sysnop,
  [1] = ...,
}

void (*)() sysno(int vno) {
  return systab[vno];
}

void syscall() {
  void (*f)();
  f = sysno(vno);
  // sysnop can be called here, because it flows in systab, which
  // then is used in sysno and return in local f and then called.
  f();
}
