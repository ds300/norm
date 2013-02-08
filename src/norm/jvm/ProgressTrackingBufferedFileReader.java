package norm.jvm;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
public class ProgressTrackingBufferedFileReader extends BufferedReader {
  private FileChannel channel;
  private long size;
  public ProgressTrackingBufferedFileReader (Reader in) {
    super(in);
  }

  public static ProgressTrackingBufferedFileReader make (String filename) {
    try {
      File thefile = new File(filename);
      long size = thefile.length();
      FileInputStream stream = new FileInputStream(thefile);
      FileChannel channel = stream.getChannel();
      ProgressTrackingBufferedFileReader ret = new ProgressTrackingBufferedFileReader(new InputStreamReader(stream));
      ret.channel = channel;
      ret.size = size;
      return ret;
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      System.exit(1);
      return null; 
    }
  }

  public float progress() {
    try {
      return (100f / size) * channel.position();
    } catch (IOException e) {return 100f;}
    
  }
}
