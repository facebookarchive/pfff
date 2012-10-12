class Anon {
  void main() {
    int x = new Anon() {
        int x;
      };

    int y = new Anon() {
        int x;
      };
  }

  void bar() {
    int x = new Anon() {
        int x;
      };

    int y = new Anon() {
        int x;
      };
  }
}