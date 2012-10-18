package gnu.classpath.jdwp.transport;
class TransportFactory {
  int _transportMethods;
  class TransportMethod {
    int clazz;
    int name;
  }
  int _TRANSPORT_PROPERTY;
}
class TransportException {
}
class SocketTransport {
  int _socket;
  int _server;
  int _host;
  int _port;
  int _PROPERTY_SERVER;
  int _PROPERTY_ADDRESS;
  int NAME;
}
class JdwpReplyPacket {
  int MINIMUM_LENGTH;
  int _errorCode;
}
class JdwpPacket {
  int _data;
  int _flags;
  int _id;
  int MINIMUM_SIZE;
  int JDWP_FLAG_REPLY;
  int _last_id;
}
class JdwpConnection {
  int _doStream;
  int _bytes;
  int _outStream;
  int _inStream;
  int _shutdown;
  int _commandQueue;
  int _transport;
  int _HANDSHAKE;
}
class JdwpCommandPacket {
  int MINIMUM_LENGTH;
  int _command;
  int _commandSet;
}
class ITransport {
}
