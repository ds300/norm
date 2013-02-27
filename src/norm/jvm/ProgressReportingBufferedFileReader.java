package norm.jvm;
import java.io.*;
import java.util.zip.*;
import org.apache.commons.io.input.CountingInputStream;

public class ProgressReportingBufferedFileReader extends BufferedReader {

  private final CountingInputStream counting_input_stream;

  private long size;
  private int iter_count;
  private final int iters = 10;
  private long last_checked = System.currentTimeMillis();
  private float last_progress = 0f;
  private float time_remaining = 0f;

  private ProgressReportingBufferedFileReader (CountingInputStream in, long filesize) {
    super(new InputStreamReader(in));
    counting_input_stream = in;
    size = filesize;

  }

  private static ProgressReportingBufferedFileReader make (InputStream in, int buffer_size, long file_size) {
    return new ProgressReportingBufferedFileReader(
      new CountingInputStream(new BufferedInputStream(in, buffer_size)),
      file_size
    );
  }

  public static ProgressReportingBufferedFileReader make (File f, int buffer_size) throws FileNotFoundException {
    return make(new FileInputStream(f), buffer_size, f.length());
  }

  public static ProgressReportingBufferedFileReader make (String path, int buffer_size) throws FileNotFoundException {
    return make(new File(path), buffer_size);
  }

  public static ProgressReportingBufferedFileReader makeGzip (File f, int buffer_size) throws FileNotFoundException, IOException {
    return make(new GZIPInputStream(new FileInputStream(f)), buffer_size, f.length());
  }

  public static ProgressReportingBufferedFileReader makeGzip (String path, int buffer_size) throws FileNotFoundException, IOException {
    return makeGzip(new File(path), buffer_size);
  }

  public String progress() {
    float prog = (100f / size) * counting_input_stream.getByteCount();
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
  }
}
