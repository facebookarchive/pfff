class AnonClassFinalParam {
  void main(final int s) {
    return new AnonClassFinalParam() {
      void foo() {
        return s;
      }
    };
  }
}