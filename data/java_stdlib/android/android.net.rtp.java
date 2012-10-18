package android.net.rtp;
class RtpStream {
  int mSocket;
  int mMode;
  int mRemotePort;
  int mRemoteAddress;
  int mLocalPort;
  int mLocalAddress;
  int MODE_LAST;
  int MODE_RECEIVE_ONLY;
  int MODE_SEND_ONLY;
  int MODE_NORMAL;
}
class AudioStream {
  int mGroup;
  int mDtmfType;
  int mCodec;
}
class AudioGroup {
  int mNative;
  int mMode;
  int mStreams;
  int MODE_LAST;
  int MODE_ECHO_SUPPRESSION;
  int MODE_NORMAL;
  int MODE_MUTED;
  int MODE_ON_HOLD;
}
class AudioCodec {
  int sCodecs;
  int AMR;
  int GSM_EFR;
  int GSM;
  int PCMA;
  int PCMU;
  int fmtp;
  int rtpmap;
  int type;
}
