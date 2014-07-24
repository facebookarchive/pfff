
// Where this interrupt function can be called?
void i850intr() {
}

void intrenable( (*f)()) {
  foo.fld = f;
}



void trap() {
  for() {
    for () {
      // i850intr can be called here because it flows in intrenable,
      // which flows in foo.fld and foo.fld is used here
      (*foo.fld)(...)
    }
  }
}

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

void main() {
  intrenable(&i850intr);
}

