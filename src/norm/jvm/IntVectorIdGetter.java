package norm.jvm;
import clojure.lang.PersistentHashMap;

/*
 * This class gets unique ids for int arrays in thread-safe goodness.
 */
public class IntVectorIdGetter {
  // this is gonna store all the ids in a trie-like fashion
  PersistentHashMap m = PersistentHashMap.EMPTY;

  // this is how many unique int arrays have been seen so far.
  private int count = 0;

  /*
   * This gets an ID if ks has been seen, and returns null if not.
   */
  public Integer get(final int[] ks) {
    Object node = m;
    for (int i=0;i<ks.length;i++) {
      if (!(node instanceof PersistentHashMap)) return null;
      node = ((PersistentHashMap)node).valAt(new Integer(ks[i]));
      if (node == null) return null;
    }
    if (node instanceof Integer) {
      return (Integer) node;
    } else {
      return null;
    }
  }

  private static PersistentHashMap put(PersistentHashMap map, int[] ks, int val) {
    if (ks.length == 0) {
      return map;
    } else if (ks.length == 1) {
      return (PersistentHashMap) map.assoc(ks[0], val);
    } else {
      PersistentHashMap child = (PersistentHashMap) map.valAt(new Integer(ks[0]));
      if (child == null) {
        child = PersistentHashMap.EMPTY;
      }
      int[] more = java.util.Arrays.copyOfRange(ks, 1, ks.length);
      return (PersistentHashMap) map.assoc(ks[0], put(child, more, val));
    }
    
  }

  /*
   * This gets an ID regardless of whether it has been seen or not.
   * new_elem_callback is called when ks has not been seen.
   */
  public synchronized int put(final int[] ks, clojure.lang.IFn new_elem_callback) {
    Integer res = get(ks);
    if (null == res) {
      if (new_elem_callback != null) {
        new_elem_callback.invoke(new Integer(count));
      }
      m = put(m, ks, count);
      count++;
      return count - 1;
    } else {
      return res;
    }
  }

  /*
   * This gets an ID regardless of whether it has been seen or not.
   */
  public synchronized int put(final int[] ks) {
    return put(ks, null);
  }
}