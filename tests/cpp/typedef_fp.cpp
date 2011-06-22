

void main() {

  if(fp_foo1) foo();

  int x = fp_foo2 * b;

  x += fp_foo4 * foo;
  x *= fp_foo5 * foo;

  real x = fp_foo6 * fp_foo7 * fb_foo8;

  x = a ? fp_foo9 * foo : bar;

  x = a.fp_foo10 * foo;
  x = a[fp_foo11 * foo];

  x = 1 - fp_foo12 * foo;

//  int id = (int) (fp_foo3 * foo());

}

template<typename T>
inline T sqr(T x) {
  return fp_foo13 * x;
}
