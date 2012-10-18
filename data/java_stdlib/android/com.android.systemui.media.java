package com.android.systemui.media;
class RingtonePlayer {
  int mCallback;
  class Client {
    int mRingtone;
    int mToken;
  }
  int mClients;
  int mAsyncPlayer;
  int mAudioService;
  int LOGD;
  int TAG;
}
class NotificationPlayer {
  int mState;
  int mAudioManager;
  int mWakeLock;
  int mPlayer;
  int mCompletionHandlingLock;
  int mCompletionThread;
  int mThread;
  int mTag;
  class CmdThread {
  }
  class CreationAndCompletionThread {
    int mCmd;
  }
  int mLooper;
  int mCmdQueue;
  class Command {
    int requestTime;
    int stream;
    int looping;
    int uri;
    int context;
    int code;
  }
  int mDebug;
  int STOP;
  int PLAY;
}
