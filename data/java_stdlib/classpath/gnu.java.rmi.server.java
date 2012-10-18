package gnu.java.rmi.server;
class UnicastServerRef {
  int methods;
  int stub;
  int skel;
  int myself;
  int stubprototype;
  int serialVersionUID;
}
class UnicastServer {
  int dgc;
  int actIds;
  int refcache;
  int objects;
}
class UnicastRemoteStub {
}
class UnicastRemoteCall {
  class DummyObjectInputStream {
  }
  class DummyObjectOutputStream {
  }
  int oin;
  int oout;
  int objid;
  int ptr;
  int vec;
  int hash;
  int opnum;
  int object;
  int result;
  int conn;
}
class UnicastRef {
  int dgcInterfaceHash;
  int DIRTY;
  int this_id;
  int dgcId;
  int dgcSequence;
  int manager;
  int objid;
  int serialVersionUID;
}
class TripleKey {
  int other;
  int port;
  int host;
}
class UnicastConnectionManager {
  int GLOBAL_LOCK;
  int debug;
  int nsmanager;
  int ncmanager;
  int nssock;
  int ncsock;
  int clientFactory;
  int serverFactory;
  int defaultSocketFactory;
  int serverobj;
  int scavenger;
  int serverPort;
  int serverName;
  int ssock;
  int serverThread;
  int connections;
  int clients;
  int servers;
  int localhost;
}
class UnicastConnection {
  int CONNECTION_TIMEOUT;
  int expireTime;
  int reviveTime;
  int oout;
  int oin;
  int dout;
  int din;
  int sock;
  int manager;
}
class RMIVoidValue {
  int INSTANCE;
}
class RMIObjectOutputStream {
}
class RMIObjectInputStream {
}
class RMIIncomingThread {
  int clientHost;
}
class RMIHashes {
}
class RMIDefaultSocketFactory {
}
class RMIClassLoaderImpl {
  int defaultCodebase;
  int defaultAnnotation;
  int defaultClassLoader;
  int cacheAnnotations;
  int cacheLoaders;
  int instance;
  class CacheKey {
    int mContextClassLoader;
    int mCodeBase;
  }
  class MyClassLoader {
    int annotation;
  }
}
class ProtocolConstants {
  int DEFAULT_PROTOCOL;
  int RETURN_NACK;
  int RETURN_ACK;
  int MESSAGE_DGCACK;
  int MESSAGE_PING_ACK;
  int MESSAGE_PING;
  int MESSAGE_CALL_ACK;
  int MESSAGE_CALL;
  int PROTOCOL_NACK;
  int PROTOCOL_ACK;
  int MULTIPLEX_PROTOCOL;
  int SINGLE_OP_PROTOCOL;
  int STREAM_PROTOCOL;
  int PROTOCOL_VERSION;
  int PROTOCOL_HEADER;
}
class ConnectionRunnerPool {
  int group;
  int freelist;
  int max_size;
  int size;
  class ConnectionRunner {
    int exiting;
    int conn;
  }
}
class CombinedClassLoader {
  int loaders;
}
class ActivatableServerRef {
  int actId;
  int serialVersionUID;
}
class ActivatableRef {
  int actId;
  int serialVersionUID;
}
