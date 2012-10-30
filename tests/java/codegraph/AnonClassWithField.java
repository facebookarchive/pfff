class AnonClass2 {
  void main() {
    return new AnonClass2() {
      int index = 0;
      
      @Override public boolean hasNext() {
        return index;
      }

    };
  }
}