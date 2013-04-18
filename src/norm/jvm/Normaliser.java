package norm.jvm;
import clojure.lang.IFn;
import java.util.List;

/*
 * This is a wrapper class for the clojure normalising fns.
 */
public class Normaliser {
  private IFn normalise_token_list;

  /**
   * This constructs a Normaliser with the given normalising fn.
   */
  public Normaliser (IFn normalise_token_list) {
    this.normalise_token_list = normalise_token_list;
  }

  /**
   * This returns a normalised version of the given token list.
   */
  public List<String> normalise(List<String> tkns) {
    return (List<String>) normalise_token_list.invoke(tkns);
  }

}
