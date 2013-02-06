package norm.trie;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Stack;
import java.util.ArrayList;
import java.util.PriorityQueue;
import java.util.List;
import clojure.lang.IPersistentMap;
import clojure.lang.IteratorSeq;
import clojure.lang.IMapEntry;
import clojure.lang.MapEntry;
import clojure.lang.IPersistentCollection;
import clojure.lang.PersistentVector;
import clojure.lang.ISeq;
import clojure.lang.Seqable;
import clojure.lang.AFn;
import clojure.lang.IFn;

public class Trie extends AFn implements IPersistentMap{
  private final long     _count;
  private final long     _freq;
  private final long     _tfreq;
  private final boolean  _terminal;
  private final char[]   _keys;       // These ones in ascending sorted order for
  private final Trie[]   _nodes;      // traversal and merging etc.
  private final Object   _data;

  public Trie () {
    _count    = 0;
    _freq     = 0;
    _tfreq    = 0;
    _terminal = false;
    _keys     = new char[0];
    _nodes    = new Trie[0];
    _data     = null;
  }

  public Trie (Trie t) {
    _count    = t._count;
    _freq     = t._freq;
    _tfreq    = t._tfreq;
    _terminal = t._terminal;
    _keys     = t._keys;
    _nodes    = t._nodes;
    _data     = t._data;
  }

  public Trie (long count, long freq, long tfreq, boolean terminal, char[] keys, Trie[] nodes, Object data) {
    _count = count;
    _freq = freq;
    _tfreq = tfreq;
    _terminal = terminal;
    _keys = keys;
    _nodes = nodes;
    _data = data;
  }



  public Trie (String word, long freq, Object data) {
    _count = 1;
    _freq = freq;
    if (word.length() == 0) {
      _terminal = true;
      _tfreq = freq;
      _data = data;
      _keys = new char[0];
      _nodes = new Trie[0];
    } else {
      _terminal = false;
      _tfreq = 0;
      _data = null;
      _keys = new char[] {word.charAt(0)};
      _nodes = new Trie[] {new Trie(word.substring(1), freq, data)};
    }
  }



  public Trie getChild (char c) {
    int i = Arrays.binarySearch(_keys, c);
    if (i >= 0) {
      return _nodes[i];
    } else {
      return null;
    }
  }

  public boolean hasChild (char c) {
    return Arrays.binarySearch(_keys, c) >= 0;
  }

  public static void swap(char[] arr, int i, int j) {
    char tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }

  public static void swap(Trie[] arr, int i, int j) {
    Trie tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }

  private static int countUnique (final char[] a, final char[] b) {
    int i = 0;
    int j = 0;
    int count = 0;
    while (i < a.length && j < b.length) {
      count++;
      if (a[i] < b[j]) { i++; }
      else if (a[i] > b[j]) { j++; }
      else { i++; j++; }
    }
    return count + (a.length - i) + (b.length - j);
  }

  public Trie merge (Trie t, IFn withfn) {
    final long count = _count + t._count;
    final long freq = _freq + t._freq;
    final long tfreq = _tfreq + t._tfreq;
    final boolean terminal = _terminal || t._terminal;
    final Object data;
    if (withfn == null) {
      data = t._data != null ? t._data : _data;
    } else {
      data = withfn.invoke(_data, t._data);
    }

    if (_keys.length == 0) {
      return new Trie(count, freq, tfreq, terminal, t._keys, t._nodes, data);
    } else if (t._keys.length == 0) {
      return new Trie(count, freq, tfreq, terminal, _keys, _nodes, data);
    } else {
      // clone and sort original data structures

      // count unique keys
      int unique = countUnique(_keys, t._keys);
      
      // create return arrays
      char[] rks = new char[unique];
      Trie[] rns = new Trie[unique];
      
      int i = 0;
      int j = 0;
      int k = 0;

      while (i < _keys.length && j < t._keys.length) {
        if (_keys[i] < t._keys[j]) {
          rks[k] = _keys[i];
          rns[k] = _nodes[i];
          i++;
        } else if (_keys[i] > t._keys[j]) {
          rks[k] = t._keys[j];
          rns[k] = t._nodes[j];
          j++;
        } else {
          rks[k] = _keys[i];
          rns[k] = _nodes[i].merge(t._nodes[j], withfn);
          i++;
          j++;
        }
        k++;
      }
      while (i < _keys.length) {
        rks[k] = _keys[i];
        rns[k] = _nodes[i];
        i++;
        k++;
      }
      while (j < t._keys.length) {
        rks[k] = t._keys[j];
        rns[k] = t._nodes[j];
        j++;
        k++;
      }
      return new Trie(count, freq, tfreq, terminal, rks, rns, data);
    }
  }

  public Trie endNode (String s) {
    Trie node = this;
    for (int i = 0; i < s.length(); i++) {
      node = node.getChild(s.charAt(i));
      if (node == null) {
        return null;
      }
    }
    return node;
  }

  public boolean contains (String s) {
    Trie node = this;
    for (int i = 0; i < s.length(); i++) {
      node = node.getChild(s.charAt(i));
      if (node == null) {
        return false;
      }
    }
    return node._terminal;
  }

  // this should only be called after it has been made certain that s is a valid key
  // in this trie
  private Trie remove (String s, long freq) {
    if (s.length() == 0) {
      // this is the terminal node of the string we're removing
      if (_count == 1) {
        // this node is useless and can be removed
        return new Trie();
      } else {
        // there's stuff coming off from this node, so just make it not terminal
        return new Trie(_count - 1, _freq - freq, 0, false, _keys, _nodes, null);
      }
    } else {
      int x = Arrays.binarySearch(_keys, s.charAt(0));
      Trie replacement = _nodes[x].remove(s.substring(1), freq);
      if (replacement._count > 0) {
        Trie[] rns = new Trie[_keys.length];
        System.arraycopy(_nodes, 0, rns, 0, _keys.length);
        rns[x] = replacement;
        return new Trie(_count - 1, _freq - freq, _tfreq, _terminal, _keys, rns, _data);
      } else {
        // do interesting shit
        char[] rks = new char[_keys.length - 1];
        Trie[] rns = new Trie[_keys.length - 1];
        System.arraycopy(_keys, 0, rks, 0, x);
        System.arraycopy(_keys, x+1, rks, x, _keys.length-1-x);
        System.arraycopy(_nodes, 0, rns, 0, x);
        System.arraycopy(_nodes, x+1, rns, x, _keys.length-1-x);
        return new Trie(_count - 1, _freq - freq, _tfreq, _terminal, rks, rns, _data);
      }
    }
  }

  /*****************************************************
  ***                                                ***
  ***              FUZZY MATCHING STUFF              ***
  ***                                                ***
  ******************************************************/

  /**
   * Finds the smallest integer in an array
   */
  private static int minInRow (int[] row) {
    int min = row[0];
    for (int i=1;i<row.length;i++) {
      if (row[i] < min) min = row[i];
    }
    return min;
  }

  /**
   * This class used in priority queues for levensthein matching
   * FindNearestEntry is so verbose
   */
  private class FNE implements Comparable {
    public int dist;
    public long freq;
    public Trie node;
    public int[] row;
    public String word;

    public FNE (int dist, long freq, String word, Trie node, int[] row) {
      this.dist = dist;
      this.freq = freq;
      this.word = word;
      this.node = node;
      this.row = row;
    }

    @Override
    public int compareTo (Object other) {
      FNE oth = (FNE) other;
      if (oth.dist > this.dist) return -1;
      if (oth.dist < this.dist) return 1;
      if (oth.freq < this.freq) return -1;
      if (oth.freq > this.freq) return -1;
      return 0;
    }
  }

  /**
   * Finds the n nearest strings to s in this trie.
   */

  public List<String> findNearest (final String s, final int n) {

    // init levenshtein stuff
    final int m = s.length();
    int[] row = new int[m + 1];
    for (int i = 0; i < m + 1; i++) row[i] = i;

    // init queue and results list
    PriorityQueue<FNE> q = new PriorityQueue<FNE>();
    q.add(new FNE(0, 0, "", this, row));
    ArrayList<String> results = new ArrayList<String>();

    FNE current = null; 

    // iterate over queue, stopping when we've got enough
    while (results.size() < n && !q.isEmpty()) {
      current = q.poll();
      if (current.node == null) {
        // if there is no node, it means we made the entry as a kind of
        // "look, this is a word we found, but we might find others which
        // are closer, so stick it back in the queue and wait for it to 
        // come around again" type of deal.
        results.add(current.word);
      } else {
        if (current.node._terminal) { // yay a legit word!
          if (current.row[m] == current.dist) {
            // if the minimum entry in the row is the \emph{last} entry
            // in the row, then it is as close to s as we're gonna get from
            // here on in, so add it to results.
            results.add(current.word);
          } else {
            // "look, this is a word we found, but we might find others which
            // are closer, so stick it back in the queue and wait for it to 
            // come around again"
            q.add(new FNE(current.row[m], current.freq, current.word, null, null));
          }
        }

        // now let's explore this node's children!

        // calculate levenshtein rows, and add new entries to the queue
        for (int i=0; i < current.node._keys.length; i++) {
          char c = current.node._keys[i];
          row = new int[m + 1];
          row[0] = current.row[0] + 1;
          for (int j=1; i < m + 1; j++) {
            row[j] =
              Math.min(
                Math.min(
                  current.row[j - 1] + (s.charAt(j - 1) == c ? 0 : 1),
                  current.row[j] + 1
                ),
                row[j - 1] + 1
              );
          }
          q.add(
            new FNE(
              minInRow(row),
              current.node._nodes[i]._freq,
              current.word + c,
              current.node._nodes[i],
              row
            )
          );
        }
      }
    }
    return results;
  }


  // CLOJURE STUFF

  @Override
  public Trie without(Object key) {
    Trie end = endNode((String) key);
    if (end == null || !end._terminal) {
      return this;
    } else {
      return remove((String) key, end._tfreq);
    }
  }

  @Override
  public Iterator iterator () {
    return new TrieIterator(this);
  }

  private class TrieIterator implements Iterator<MapEntry> {
    private class Item {
      public String acc;
      public Trie node;
      public int child_id = 0;
      public boolean done = false;
      public Item (String acc, Trie node) {
        this.acc = acc;
        this.node = node;
      }
    }
    private Stack<Item> stack = new Stack<Item>();
    private MapEntry head;
    public TrieIterator (Trie root) {
      stack.push(new Item("", root));
      head = _next();
    }
    @Override
    public boolean hasNext () {
      return head != null;
    }
    @Override
    public MapEntry next() {
      MapEntry v = head;
      head = _next();
      return v;
    }
    private MapEntry _next () {
      if (stack.empty()) return null;
      Item item = stack.peek();
      if (!item.done && item.node._terminal) {
        item.done = true;
        return new MapEntry(item.acc, PersistentVector.create(new Object[]{new Long(item.node._tfreq), item.node._data}));
      } else if (item.child_id >= item.node._nodes.length) {
        stack.pop();
        return _next();
      } else {
        stack.push(new Item(item.acc + item.node._keys[item.child_id],
                            item.node._nodes[item.child_id]));
        item.child_id++;
        return _next();
      }
    }
    @Override
    public void remove () {}
  }

  public void printTrie () {
    printTrie(this, 0);
  }

  private void printTrie (Trie t, int depth) {
    for (int i = 0; i < t._keys.length; i++) {
      for (int j = 0; j < depth; j++) {
        System.out.print("|   ");
      }
      System.out.println(t._keys[i]);
      printTrie(t._nodes[i], depth + 1);
    }
  }

  @Override
  public IMapEntry entryAt(Object key) {
    return new MapEntry(key, valAt(key));
  }


  @Override
  public IPersistentMap assoc(Object key, Object val) {
    return merge(new Trie((String) key, 0, val), null);
  }

  @Override
  public IPersistentMap assocEx(Object key, Object val) {
    return assoc(key, val);
  }

  @Override
  public boolean containsKey(Object key) {
    return contains((String) key);
  }

  @Override
  public int count() { return (int)_count; };

  @Override
  public IPersistentCollection cons(Object o) {
    ISeq s = ((Seqable) o).seq();
    String word = (String) s.first();
    s = s.next();
    if (s.first() instanceof Number) {
      return merge(new Trie(word, (Long)s.first(), s.next() == null ? null : s.next().first()), null);
    } else {
      return merge(new Trie(word, 0, s.first()), null);
    }
  }

  @Override
  public IPersistentCollection empty() {
    return new Trie();
  }

  @Override
  public boolean equiv(Object o) {
    try {
      Trie t = (Trie) o;
      if (t == this) return true;
      if (t._count != _count || _freq != t._freq || _tfreq != t._tfreq || t._terminal != _terminal || t._keys.length != _keys.length) return false;
      for (int i = 0; i < _keys.length; i++) {
        if (t._keys[i] != _keys[i]) return false;
        if (!t._nodes[i].equiv(_nodes[i])) return false;
      }
      if (t._data != _data) {
        if (t._data != null && _data != null && t._data.equals(_data)) {
          return true;
        } else {
          return false;
        }
      }
      return true;
    } catch (ClassCastException e) {
      return false;
    }

  }

  @Override
  public ISeq seq () {
    return IteratorSeq.create(iterator());
  }

  public Long freq (String prefix) {
    Trie end = endNode(prefix);
    return end == null ? 0 : end._freq;
  }

  public Long tfreq (String prefix) {
    Trie end = endNode(prefix);
    return end == null ? 0 : end._tfreq;
  }

  // public Long freq (String prefix, Object notFound) {
  //   Trie end = endNode(prefix);
  //   return end == null ? notFound : end._freq;
  // }

  // public Long tfreq (String prefix, Object notFound) {
  //   Trie end = endNode(prefix);
  //   return end == null ? notFound : end._tfreq;
  // }

  @Override
  public Object valAt (Object key) {
    Trie end = endNode((String) key);
    return end == null ? null : end._data;
  }

  @Override
  public Object valAt (Object key, Object notFound) {
    Trie end = endNode((String) key);
    return end == null ? notFound : end._data;
  }

  @Override
  public Object invoke(Object arg1) {
    return valAt(arg1);
  }

  @Override
  public Object invoke(Object arg1, Object notFound) {
    return valAt(arg1, notFound);
  }

  public static void printArr(char[] arr) {
    if (arr.length == 0) {
      System.out.println("[]");
    } else {
      System.out.print("[" + arr[0]);
      for (int i = 1; i < arr.length; i++) {
        System.out.print(", " + arr[i]);
      }
      System.out.println("]");
    }
  }

  public static void printArr(Trie[] arr) {
    if (arr.length == 0) {
      System.out.println("[]");
    } else {
      System.out.print("[" + arr[0]);
      for (int i = 1; i < arr.length; i++) {
        System.out.print(", " + arr[i]);
      }
      System.out.println("]");
    }
  }

  @Override
  public String toString() {
    return "{count " + _count + ", freq " + _freq + "}";
  }

  @Override
  public boolean equals(Object o) {
    return equiv(o);
  }


}