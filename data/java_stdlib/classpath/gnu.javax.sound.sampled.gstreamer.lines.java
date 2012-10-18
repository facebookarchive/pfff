package gnu.javax.sound.sampled.gstreamer.lines;
class GstSourceDataLine {
  int open;
  int pipeline;
}
class GstPipeline {
  class CleanPipeline {
  }
  int pipeline;
  int ready;
  int source;
  int output;
  int name;
  int state;
  int capacity;
  int prefs;
  int lock;
  int CAPACITY_KEY;
  int QUEUED;
  int WRITE;
  int READ;
  class State {
    int CLOSE;
    int STOP;
    int PAUSE;
    int PLAY;
  }
}
class GstNativeDataLine {
}
class GstDataLine {
  int bufferSize;
  int format;
  int open;
  int DEFAULT_BUFFER_SIZE;
}
