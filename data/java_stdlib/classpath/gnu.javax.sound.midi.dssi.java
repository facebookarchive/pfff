package gnu.javax.sound.midi.dssi;
class DSSISynthesizer {
  int defaultSoundbank;
  int soundbanks;
  int channels;
  int info;
  int handle;
  int sohandle;
  class DSSIMidiChannel {
    int channel;
  }
  class DSSIReceiver {
  }
  class DSSISoundbank {
    int version;
    int vendor;
    int resources;
    int instruments;
    int description;
    int name;
  }
  class DSSIInstrument {
  }
}
class DSSIMidiDeviceProvider {
  int infos;
  class DSSIInfo {
    int index;
    int soname;
    int copyright;
  }
}
