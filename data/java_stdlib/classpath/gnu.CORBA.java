package gnu.CORBA;
class gnuValueHolder {
  int helper_NA;
  int helper;
  int type;
}
class gnuRequest {
  int redirected;
  int Big_endian;
  int orb;
  int ior;
  int m_rph;
  int m_rqh;
  int m_slots;
  int m_parameter_buffer;
  int m_args;
  int running;
  int oneWay;
  int complete;
  int m_forwarding_target;
  int m_forward_ior;
  int m_operation;
  int m_target;
  int m_sys_ex;
  int m_exception_id;
  int m_result;
  int m_exceptions;
  int m_environment;
  int m_context_list;
  int m_context;
  int EMPTY;
  int m_info;
  int m_interceptor;
  int PAUSE_MAX;
  int PAUSE_STEPS;
  int PAUSE_INITIAL;
  int MAX_SUPPORTED;
}
class gnuNamedValue {
  int m_flags;
  int m_name;
  int m_value;
}
class gnuNVList {
  int list;
}
class gnuExceptionList {
  int list;
}
class gnuEnvironment {
  int exception;
}
class gnuContextList {
  int strings;
}
class gnuContext {
  int name;
  int properties;
  int parent;
}
class gnuCodecFactory {
  int orb;
}
class gnuAny {
  int orb;
  int xKind;
  int typecode;
  int has;
  int nullType;
  int serialVersionUID;
}
class _PolicyImplBase {
  int policyCode;
  int value;
  int type;
  int ids;
  int serialVersionUID;
}
class WStringHolder {
  int value;
  int t_string;
}
class WCharHolder {
  int value;
  int t_char;
}
class Version {
  int minor;
  int major;
  int serialVersionUID;
}
class Unexpected {
  int SHARED_MESSAGE;
  int serialVersionUID;
}
class TypeKindNamer {
  int primitveCodes;
  int tk;
}
class TypeCodeHelper {
}
class StubLocator {
}
class StreamHolder {
  int stream;
}
class StreamBasedRequest {
  int response_expected;
  int request;
}
class SocketRepository {
  int sockets;
}
class SimpleDelegate {
  int ior;
  int orb;
}
class SetOverrideTypeHolder {
  int value;
}
class ServiceRequestAdapter {
  int isException;
  int reply;
}
class ServiceDetailHolder {
  int value;
}
class SafeForDirectCalls {
}
class ResponseHandlerImpl {
  int buffer;
  int exceptionReply;
  int request_header;
  int reply_header;
  int orb;
  int message_header;
}
class RawReply {
  int data;
  int orb;
  int header;
}
class OrbRestricted {
  int policyFactories;
  int factories;
  int icSlotSize;
  int iClient;
  int iServer;
  int iIor;
  int Singleton;
}
class OrbFunctional {
  int socketFactory;
  int MAX_RUNNING_THREADS;
  int identities;
  int freed_ports;
  int asynchron;
  int nameParser;
  int ns_port;
  int Port;
  int ns_host;
  int portServers;
  int initial_references;
  int running;
  int max_version;
  int connected_objects;
  int server_id;
  int orb_id;
  int TANDEM_REQUESTS;
  int TWAIT_SERVER_ERROR_PAUSE;
  int TOUT_AFTER_RECEIVING;
  int TOUT_WHILE_READING;
  int TOUT_START_READING_MESSAGE;
  int LOCAL_HOST;
  int SERVER_ERROR_PAUSE;
  int AFTER_RECEIVING;
  int WHILE_READING;
  int START_READING_MESSAGE;
  int SERVER_ID;
  int ORB_ID;
  int NAME_SERVICE;
  int NS_HOST;
  int NS_PORT;
  int REFERENCE;
  int LISTEN_ON;
  int RANDOM_PORT_ATTEMPTS;
  int RANDOM_PORT_TO;
  int RANDOM_PORT_FROM;
  int DEFAULT_INITIAL_PORT;
  class sharedPortServer {
  }
  class portServer {
    int terminated;
    int service;
    int s_port;
    int running_threads;
  }
}
class OrbFocused {
  int m_random;
  int SEQUENTIAL;
  int PARALLEL;
  int m_ports_to;
  int m_ports_from;
  int LISTENER_PORT;
}
class OctetHolder {
  int value;
  int t_octet;
}
class ObjectCreator {
  int m_helpers;
  int m_classes;
  int m_names;
  int CLASSPATH_PREFIX;
  int JAVA_PREFIX;
  int OMG_PREFIX;
}
class NameValuePairSeqHolder {
  int value;
}
class NameValuePairHolder {
  int value;
}
class NameDynAnyPairSeqHolder {
  int value;
}
class NameDynAnyPairHolder {
  int value;
}
class Minor {
  int Missing_IOR;
  int Threads;
  int Ports;
  int PolicyType;
  int Enumeration;
  int Socket;
  int Policy;
  int Activation;
  int Method;
  int NonSerializable;
  int ValueFields;
  int TargetConversion;
  int IOR;
  int UnsupportedAddressing;
  int Factory;
  int UnsupportedValue;
  int Chunks;
  int Offset;
  int ClassCast;
  int ValueHeaderFlags;
  int ValueHeaderTag;
  int Instantiation;
  int Boxed;
  int Graph;
  int Negative;
  int Inappropriate;
  int UserException;
  int Any;
  int Encapsulation;
  int Forwarding;
  int Value;
  int CDR;
  int EOF;
  int Header;
  int Giop;
  int vendor;
}
class IorProvider {
}
class IorObject {
  int id;
  int ior;
}
class IorDelegate {
}
class IOR {
  int Big_Endian;
  int profiles;
  int key;
  int Id;
  int Internet;
  int FAILED;
  class Internet_profile {
    int components;
    int CodeSets;
    int port;
    int version;
    int host;
    int TAG_INTERNET_IOP;
  }
  class CodeSets_profile {
    int negotiated;
    int wide;
    int narrow;
    int TAG_CODE_SETS;
    class CodeSet_component {
      int native_set;
      int conversion;
    }
  }
}
class HolderLocator {
  int seqHolders;
  int holders;
}
class GeneralHolder {
  int value;
}
class ForwardRequestHelper {
}
class EmptyExceptionHolder {
  int typecode;
  int value;
}
class DynAnySeqHolder {
  int value;
}
class DuplicateNameHolder {
  int value;
}
class DefinitionKindHolder {
  int value;
}
class DefaultSocketFactory {
  int Singleton;
}
class CorbaList {
  int serialVersionUID;
}
class Connected_objects {
  int objects;
  int free_object_number;
  class cObject {
    int identity;
    int key;
    int port;
    int object;
  }
}
class CollocatedOrbs {
  int localHost;
  int orbs;
  int DIRECT_CALLS_ALLOWED;
}
class CdrEncapsCodecImpl {
  int lengthIndicator;
  int orb;
  int version;
  int noWide;
  int serialVersionUID;
}
class ByteArrayComparator {
}
class BigDecimalHelper {
}
class Asynchron {
  int sent;
}
