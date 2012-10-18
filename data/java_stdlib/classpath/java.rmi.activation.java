package java.rmi.activation;
class UnknownObjectException {
  int serialVersionUID;
}
class UnknownGroupException {
  int serialVersionUID;
}
class Activator {
}
class ActivationSystem {
  int SYSTEM_PORT;
}
class ActivationMonitor {
}
class ActivationInstantiator {
}
class ActivationID {
  int group;
  int uid;
  int activator;
  int serialVersionUID;
}
class ActivationGroup_Stub {
  int serialVersionUID;
}
class ActivationGroupID {
  int uid;
  int system;
  int serialVersionUID;
}
class ActivationGroupDesc {
  int hash;
  int props;
  int env;
  int data;
  int location;
  int className;
  int serialVersionUID;
  class CommandEnvironment {
    int options;
    int command;
    int NO_ARGS;
    int serialVersionUID;
  }
}
class ActivationGroup {
  int cConstructorTypes;
  int system;
  int incarnation;
  int monitor;
  int groupId;
  int currentGroupId;
  int serialVersionUID;
}
class ActivationException {
  int detail;
  int serialVersionUID;
}
class ActivationDesc {
  int restart;
  int data;
  int location;
  int classname;
  int groupid;
  int serialVersionUID;
}
class ActivateFailedException {
  int serialVersionUID;
}
class Activatable {
  int id;
  int serialVersionUID;
}
