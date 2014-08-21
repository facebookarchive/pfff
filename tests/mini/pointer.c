int global;

void main () {
  int x; 
  int y;

  int *p;
  int *w;
  int **q;
  int* z;

  p = &x;
  q = &p;
  w = &y;
  *q = w; // big effect
  w = p;
  y = global;
  z = *q;
}

