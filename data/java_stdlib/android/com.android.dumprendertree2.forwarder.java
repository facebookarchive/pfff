package com.android.dumprendertree2.forwarder;
class ForwarderManager {
  int mIsStarted;
  int mForwarders;
  int forwarderManager;
  int HTTPS_PORT;
  int HTTP_PORT;
  int HOST_IP;
  int LOG_TAG;
}
class Forwarder {
  int mConnectionHandlers;
  int mServerSocket;
  int mRemoteMachineIpAddress;
  int mPort;
  int LOG_TAG;
}
class ConnectionHandler {
  int mOnFinishedCallback;
  int mRemoteMachineIpAddress;
  int mPort;
  int mToSocketOutputStream;
  int mFromSocketOutputStream;
  int mToSocketInputStream;
  int mFromSocketInputStream;
  int mToFromPipe;
  int mFromToPipe;
  int mToSocket;
  int mFromSocket;
  int mThreadsRunning;
  class SocketPipeThread {
    int mOutputStream;
    int mInputStream;
  }
  class OnFinishedCallback {
  }
  int LOG_TAG;
}
class AdbUtils {
  int ADB_RESPONSE_SIZE;
  int ADB_HOST;
  int ADB_PORT;
  int ADB_OK;
  int LOG_TAG;
}
