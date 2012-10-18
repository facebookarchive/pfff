package org.apache.harmony.luni.util;
class TwoKeyHashMap {
  class ValueIteratorImpl {
    int itr;
  }
  class ValuesCollectionImpl {
  }
  class EntryIteratorImpl {
    int returned_entry;
    int curr_entry;
    int returned_index;
    int curr;
    int found;
    int startModCount;
  }
  class EntrySetImpl {
  }
  class Entry {
    int next;
    int value;
    int key2;
    int key1;
    int hash;
  }
  int threshold;
  int loadFactor;
  int arr;
  int modCount;
  int arrSize;
  int size;
  int values;
  int entrySet;
  int DEFAULT_INITIAL_SIZE;
  int DEFAULT_LOAD_FACTOR;
}
class DeleteOnExit {
  int files;
  int instance;
}
