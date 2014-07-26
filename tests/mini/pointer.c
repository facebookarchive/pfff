//todo: examples taken from Andersen thesis

void main () {
  int x; int y;
  int *p;
  int *w;
  int **q;
  p = &x;
  q = &p;
  w = &y;
  *q = w; // big effect
  w = p;
}

