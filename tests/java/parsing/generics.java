// http://en.wikipedia.org/wiki/Generics_in_Java

public class Entry<K, V> {
 
  private final K key;
  private final V value;
 
  public Entry(K k,V v) {  
    key = k;
    value = v;   
  }
 
  public K getKey() {
    return key;
  }
 
  public V getValue() {
    return value;
  }
 
  public String toString() { 
    return "(" + key + ", " + value + ")";  
  }
}