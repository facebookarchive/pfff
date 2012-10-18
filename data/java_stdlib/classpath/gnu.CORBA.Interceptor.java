package gnu.CORBA.Interceptor;
class gnuServerRequestInfo {
  int m_request;
  int m_usr_exception;
  int m_sys_exception;
  int m_forward_reference;
  int m_reply_header;
  int m_request_header;
  int m_slots;
  int not_available;
  int m_object;
  int serialVersionUID;
}
class gnuIorInfo {
  int ior;
  int poa;
  int orb;
  int serialVersionUID;
}
class gnuIcCurrent {
  int NO_SLOTS;
  int threads;
  int orb;
  int serialVersionUID;
}
class gnuClientRequestInfo {
  int m_wrapped_exception;
  int request;
  int serialVersionUID;
}
class ServerRequestInterceptors {
  int interceptors;
}
class Registrator {
  int m_codecFactory;
  int m_args;
  int orb;
  int m_initializers;
  int m_references;
  int m_policyFactories;
  int m_ior;
  int m_client;
  int m_server;
  int m_prefix;
  int serialVersionUID;
}
class IORInterceptors {
  int interceptors;
}
class ForwardRequestHolder {
  int value;
}
class ClientRequestInterceptors {
  int interceptors;
}
