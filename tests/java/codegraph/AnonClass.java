package X;

class AnonClass {
  void main() {
    int x = new AnonClass() {
        int x;
      };

    int y = new AnonClass() {
        int x;
      };
  }

  void bar() {
    int x = new AnonClass() {
        int x;
      };

    int y = new AnonClass() {
        int x;
      };
  }
}