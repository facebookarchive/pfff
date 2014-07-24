

void i850intr() {
}
intrenable(i850intr);

void intrenable( (*f)()) {
  foo.fld = f;
}

void trap() {
  for() {
    for () {
      (*foo.fld)(...)
    }
  }
}


void sysnop() {
}

void systab[] = {
  [0] = sysnop,
}

void syscall() {
  f = sysno(vno);
  f();
}
