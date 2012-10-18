package android.security;
class SystemKeyStore {
  int mInstance;
  int KEY_FILE_EXTENSION;
  int SYSTEM_KEYSTORE_DIRECTORY;
}
class KeyStore {
  int mError;
  int sAddress;
  class State {
    int UNINITIALIZED;
    int LOCKED;
    int UNLOCKED;
  }
  int WRONG_PASSWORD;
  int UNDEFINED_ACTION;
  int VALUE_CORRUPTED;
  int KEY_NOT_FOUND;
  int PERMISSION_DENIED;
  int PROTOCOL_ERROR;
  int SYSTEM_ERROR;
  int UNINITIALIZED;
  int LOCKED;
  int NO_ERROR;
}
class KeyChainException {
}
class KeyChainAliasCallback {
}
class KeyChain {
  class KeyChainConnection {
    int service;
    int serviceConnection;
    int context;
  }
  class AliasResponse {
    int keyChainAliasResponse;
  }
  int ACTION_STORAGE_CHANGED;
  int EXTRA_PKCS12;
  int EXTRA_CERTIFICATE;
  int EXTRA_NAME;
  int ACTION_INSTALL;
  int EXTRA_SENDER;
  int EXTRA_ALIAS;
  int EXTRA_PORT;
  int EXTRA_HOST;
  int EXTRA_RESPONSE;
  int ACTION_CHOOSER;
  int ACCOUNT_TYPE;
  int TAG;
}
class Credentials {
  int singleton;
  int EXTRA_CA_CERTIFICATES_DATA;
  int EXTRA_CA_CERTIFICATES_NAME;
  int EXTRA_USER_CERTIFICATE_DATA;
  int EXTRA_USER_CERTIFICATE_NAME;
  int EXTRA_USER_PRIVATE_KEY_DATA;
  int EXTRA_USER_PRIVATE_KEY_NAME;
  int EXTENSION_PFX;
  int EXTENSION_CER;
  int EXTENSION_P12;
  int EXTENSION_CRT;
  int EXTRA_PRIVATE_KEY;
  int EXTRA_PUBLIC_KEY;
  int WIFI;
  int VPN;
  int USER_PRIVATE_KEY;
  int USER_CERTIFICATE;
  int CA_CERTIFICATE;
  int UNLOCK_ACTION;
  int INSTALL_ACTION;
  int LOGTAG;
}
