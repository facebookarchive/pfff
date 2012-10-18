package gnu.CORBA.Poa;
class gnuThreadPolicy {
  int serialVersionUID;
}
class gnuServantRetentionPolicy {
  int serialVersionUID;
}
class gnuServantObject {
  int noRetain;
  int repository_ids;
  int orb;
  int manager;
  int poa;
  int Id;
  int servant;
}
class gnuRequestProcessingPolicy {
  int serialVersionUID;
}
class gnuPoaCurrent {
  int threads;
}
class gnuPOAManager {
  int state;
  int POAs;
  int serialVersionUID;
}
class gnuPOA {
  int m_object_factory;
  int retain_servant;
  int m_inDestruction;
  int m_orb;
  int s_policies;
  int m_policies;
  int m_poa_id;
  int default_servant;
  int servant_locator;
  int servant_activator;
  int m_manager;
  int m_activator;
  int SIGNATURE;
  int parent;
  int name;
  int children;
  int aom;
  int ref_template_ids;
  int refTemplate;
  int serialVersionUID;
  class RefTemplate {
    int m_adapter_name;
    int serialVersionUID;
  }
}
class gnuLifespanPolicy {
  int serialVersionUID;
}
class gnuImplicitActivationPolicy {
  int serialVersionUID;
}
class gnuIdUniquenessPolicy {
  int serialVersionUID;
}
class gnuIdAssignmentPolicy {
  int serialVersionUID;
}
class gnuForwardRequest {
  int forwarding_code;
  int forward_reference;
  int serialVersionUID;
}
class gnuAdapterActivator {
}
class StandardPolicies {
  int rootPOASet;
}
class ServantDelegateImpl {
  int object;
  int poa;
  int servant_id;
  int servant;
}
class ORB_1_4 {
  int factory;
  int ic_current;
  int currents;
  int rootPOA;
}
class LocalServerRequest {
  int request;
}
class LocalRequest {
  int serverRequest;
  int object;
  int poa;
  int buffer;
  int exceptionReply;
  int header;
  int Id;
  int cookie;
}
class LocalDelegate {
  int Id;
  int poa;
  int operation;
  int object;
}
class InvalidPolicyHolder {
  int value;
}
class ForwardedServant {
  int ref;
}
class ForwardRequestHolder {
  int value;
}
class DynamicImpHandler {
  int servant;
}
class AccessiblePolicy {
}
class AOM {
  int objects;
  int free_id;
  class Obj {
    int deactivated;
    int key;
    int poa;
    int primary_servant;
    int servant;
    int object;
  }
}
