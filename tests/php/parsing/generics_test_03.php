<?hh

class Foo {
  public Vector< ?Vector<string> > $y;
  public Vector<?Vector<string> > $y;
  public Vector<Vector<string>> $y;

  // not handled for now :(
  // public Vector< ?Vector<string>> $y;
}
