package java.beans.beancontext;
class BeanContextSupport {
  int serializing;
  int okToUseGui;
  int locale;
  int designTime;
  int children;
  int bcmListeners;
  class BCSIterator {
    int child;
  }
  class BCSChild {
    int peer;
    int targetChild;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class BeanContextServicesSupport {
  int serviceLeases;
  int serviceRequests;
  int serviceUsers;
  int services;
  int serializable;
  int proxy;
  int bcsListeners;
  class ServiceLease {
    int service;
  }
  class ServiceRequest {
    int listener;
  }
  class Request {
    int requestor;
  }
  class BCSSServiceProvider {
    int serviceClass;
    int serviceProvider;
    int serialVersionUID;
  }
  class BCSSProxyServiceProvider {
    int provider;
    int serialVersionUID;
  }
  class BCSSChild {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class BeanContextServicesListener {
}
class BeanContextServices {
}
class BeanContextServiceRevokedListener {
}
class BeanContextServiceRevokedEvent {
  int invalidateRefs;
  int serviceClass;
  int serialVersionUID;
}
class BeanContextServiceProviderBeanInfo {
}
class BeanContextServiceProvider {
}
class BeanContextServiceAvailableEvent {
  int serviceClass;
  int serialVersionUID;
}
class BeanContextProxy {
}
class BeanContextMembershipListener {
}
class BeanContextMembershipEvent {
  int children;
  int serialVersionUID;
}
class BeanContextEvent {
  int propagatedFrom;
  int serialVersionUID;
}
class BeanContextContainerProxy {
}
class BeanContextChildSupport {
  int vcSupport;
  int pcSupport;
  int rejectedSetBCOnce;
  int beanContext;
  int beanContextChildPeer;
  int serialVersionUID;
}
class BeanContextChildComponentProxy {
}
class BeanContextChild {
}
class BeanContext {
  int globalHierarchyLock;
}
