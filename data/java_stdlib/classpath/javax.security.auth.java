package javax.security.auth;
class SubjectDomainCombiner {
  int subject;
}
class Subject {
  class SecureSet {
    int type;
    int elements;
    int subject;
    int PRIVATE_CREDENTIALS;
    int PUBLIC_CREDENTIALS;
    int PRINCIPALS;
    int serialVersionUID;
  }
  int privCred;
  int pubCred;
  int readOnly;
  int principals;
  int serialVersionUID;
}
class Refreshable {
}
class RefreshFailedException {
}
class PrivateCredentialPermission {
  class CredOwner {
    int principalName;
    int principalClass;
  }
  int testing;
  int principals;
  int credentialClass;
  int serialVersionUID;
}
class Policy {
  int policy;
}
class Destroyable {
}
class DestroyFailedException {
}
class AuthPermission {
}
