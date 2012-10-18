package javax.security.auth.login;
class NullConfiguration {
}
class LoginException {
  int serialVersionUID;
}
class LoginContext {
  int sharedState;
  int modules;
  int entries;
  int subject;
  int cbHandler;
  int name;
  int OTHER;
}
class FailedLoginException {
  int serialVersionUID;
}
class CredentialNotFoundException {
  int serialVersionUID;
}
class CredentialExpiredException {
  int serialVersionUID;
}
class CredentialException {
  int serialVersionUID;
}
class Configuration {
  int config;
}
class AppConfigurationEntry {
  class LoginModuleControlFlag {
    int SUFFICIENT;
    int REQUISITE;
    int REQUIRED;
    int OPTIONAL;
  }
  int options;
  int controlFlag;
  int loginModuleName;
}
class AccountNotFoundException {
  int serialVersionUID;
}
class AccountLockedException {
  int serialVersionUID;
}
class AccountExpiredException {
  int serialVersionUID;
}
class AccountException {
  int serialVersionUID;
}
