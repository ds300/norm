package norm.jvm;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.zip.*;
public class ProgressTrackingBufferedFileReader extends BufferedReader {
  private FileChannel channel;
  private long size;
  private int iter_count;
  private final int iters = 10;
  private long last_checked = System.currentTimeMillis();
  private float last_progress = 0f;
  private float time_remaining = 0f;
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

  public static ProgressTrackingBufferedFileReader makeGzip (String filename) {
    try {
      File thefile = new File(filename);
      long size = thefile.length();
      FileInputStream stream = new FileInputStream(thefile);
      FileChannel channel = stream.getChannel();
      ProgressTrackingBufferedFileReader ret = new ProgressTrackingBufferedFileReader(new InputStreamReader(new GZIPInputStream(stream)));
      ret.channel = channel;
      ret.size = size;
      return ret;
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      System.exit(1);
      return null; 
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
      return null;
    } 
  }

  public String progress() {

    try {
      float prog = (100f / size) * channel.position();
      if (++iter_count == iters) {
        long current_time = System.currentTimeMillis();
        long time = current_time - last_checked;
        float speed = (prog - last_progress) / time;
        time_remaining = ((100f - prog) / speed) / 1000;
        last_checked = current_time;
        last_progress = prog;
        iter_count = 0;
      }
      long hours = (int)time_remaining / (60*60);
      long minutes = (int)(time_remaining / 60) % 60;
      long seconds = (int)time_remaining % 60;
      return String.format("%.2f%% done, %dh%dm%ds remaining", prog, hours, minutes, seconds);
    } catch (IOException e) {return "100.00% done 0h0m0s remaining";}
  }
}
