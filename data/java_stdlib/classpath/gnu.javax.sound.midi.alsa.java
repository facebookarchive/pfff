package gnu.javax.sound.midi.alsa;
class AlsaPortDevice {
  class AlsaTransmitter {
    int receiver;
  }
  int info;
  class AlsaReceiver {
  }
}
class AlsaOutputPortDevice {
}
class AlsaMidiSequencerDevice {
  int sequence;
  int nativeState;
  int instance;
}
class AlsaMidiDeviceProvider {
  int infos;
  class AlsaOutputPortInfo {
  }
  class AlsaInputPortInfo {
  }
  class AlsaSequencerInfo {
  }
  class AlsaPortInfo {
    int port;
    int client;
  }
  class AlsaInfo {
  }
}
class AlsaInputPortDevice {
}
