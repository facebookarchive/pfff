package javax.sound.midi;
class VoiceStatus {
  int volume;
  int note;
  int program;
  int bank;
  int channel;
  int active;
}
class Transmitter {
}
class Track {
  int eventSet;
  int events;
}
class SysexMessage {
  int SPECIAL_SYSTEM_EXCLUSIVE;
  int SYSTEM_EXCLUSIVE;
}
class Synthesizer {
}
class SoundbankResource {
  int dataClass;
  int name;
  int soundbank;
}
class Soundbank {
}
class ShortMessage {
  int defaultMessage;
  int PITCH_BEND;
  int CHANNEL_PRESSURE;
  int PROGRAM_CHANGE;
  int CONTROL_CHANGE;
  int POLY_PRESSURE;
  int NOTE_ON;
  int NOTE_OFF;
  int SYSTEM_RESET;
  int ACTIVE_SENSING;
  int STOP;
  int CONTINUE;
  int START;
  int TIMING_CLOCK;
  int END_OF_EXCLUSIVE;
  int TUNE_REQUEST;
  int SONG_SELECT;
  int SONG_POSITION_POINTER;
  int MIDI_TIME_CODE;
}
class Sequencer {
  class SyncMode {
    int name;
    int NO_SYNC;
    int MIDI_TIME_CODE;
    int MIDI_SYNC;
    int INTERNAL_CLOCK;
  }
}
class Sequence {
  int SMPTE_30DROP;
  int SMPTE_30;
  int SMPTE_25;
  int SMPTE_24;
  int PPQ;
  int tracks;
  int resolution;
  int divisionType;
}
class Receiver {
}
class Patch {
  int program;
  int bank;
}
class MidiUnavailableException {
  int serialVersionUID;
}
class MidiSystem {
}
class MidiMessage {
  int length;
  int data;
}
class MidiFileFormat {
  int UNKNOWN_LENGTH;
  int microsecondLength;
  int byteLength;
  int resolution;
  int divisionType;
  int type;
}
class MidiEvent {
  int tick;
  int message;
}
class MidiDevice {
  class Info {
    int version;
    int description;
    int vendor;
    int name;
  }
}
class MidiChannel {
}
class MetaMessage {
  int lengthLength;
  int META;
}
class MetaEventListener {
}
class InvalidMidiDataException {
  int serialVersionUID;
}
class Instrument {
  int patch;
}
class ControllerEventListener {
}
