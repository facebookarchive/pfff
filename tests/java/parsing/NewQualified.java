public class NewQualified {
  public class Inner {
    private int x;
    public Inner(int x) {
      this.x = x;
    }
    // clowntown here
    public int getX() {
      return this.x;
    }
  }

  public static final void main(String[] args) {
    NewQualified x = new NewQualified();

    Inner y = x.new Inner(5);

    System.out.println(y.getX()); // prints "5"
  }
}