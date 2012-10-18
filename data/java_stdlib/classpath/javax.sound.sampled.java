package javax.sound.sampled;
class UnsupportedAudioFileException {
  int serialVersionUID;
}
class TargetDataLine {
}
class SourceDataLine {
}
class ReverbType {
  int decayTime;
  int lateReflectionIntensity;
  int lateReflectionDelay;
  int earlyReflectionIntensity;
  int earlyReflectionDelay;
  int name;
}
class Port {
  class Info {
    int isSource;
    int name;
    int SPEAKER;
    int MICROPHONE;
    int LINE_OUT;
    int LINE_IN;
    int HEADPHONE;
    int COMPACT_DISC;
  }
}
class Mixer {
  class Info {
    int version;
    int vendor;
    int description;
    int name;
  }
}
class LineUnavailableException {
  int serialVersionUID;
}
class LineListener {
}
class LineEvent {
  int line;
  int framePosition;
  int type;
  class Type {
    int name;
    int STOP;
    int START;
    int OPEN;
    int CLOSE;
  }
  int serialVersionUID;
}
class Line {
  class Info {
    int klass;
  }
}
class FloatControl {
  int midLabel;
  int maxLabel;
  int minLabel;
  int units;
  int value;
  int updatePeriod;
  int precision;
  int maximum;
  int minimum;
  class Type {
    int VOLUME;
    int SAMPLE_RATE;
    int REVERB_SEND;
    int REVERB_RETURN;
    int PAN;
    int MASTER_GAIN;
    int BALANCE;
    int AUX_SEND;
    int AUX_RETURN;
  }
}
class EnumControl {
  int value;
  int values;
  class Type {
    int REVERB;
  }
}
class DataLine {
  class Info {
    int formats;
    int maxBufferSize;
    int minBufferSize;
  }
}
class Control {
  int type;
  class Type {
    int name;
  }
}
class CompoundControl {
  int memberControls;
  class Type {
  }
}
class Clip {
  int LOOP_CONTINUOUSLY;
}
class BooleanControl {
  int falseLabel;
  int trueLabel;
  int value;
  class Type {
    int MUTE;
    int APPLY_REVERB;
  }
}
class AudioSystem {
  int NOT_SPECIFIED;
}
class AudioPermission {
  int serialVersionUID;
}
class AudioInputStream {
  class TargetInputStream {
    int buf;
    int line;
  }
  int markedFramePos;
  int input;
  int frameSize;
  int framePos;
  int frameLength;
  int format;
}
class AudioFormat {
  int properties;
  int sampleSizeInBits;
  int sampleRate;
  int frameSize;
  int frameRate;
  int encoding;
  int channels;
  int bigEndian;
  class Encoding {
    int name;
    int ULAW;
    int PCM_UNSIGNED;
    int PCM_SIGNED;
    int ALAW;
  }
}
class AudioFileFormat {
  int properties;
  int frameLength;
  int type;
  int format;
  int byteLength;
  class Type {
    int extension;
    int name;
    int WAVE;
    int SND;
    int AU;
    int AIFF;
    int AIFC;
  }
}
