package gnu.classpath.tools.keytool;
class StorePasswdCmd {
  int newStorePasswordChars;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _newPassword;
  int log;
}
class SelfCertCmd {
  int validityInDays;
  int distinguishedName;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _validityStr;
  int _password;
  int _dName;
  int _sigAlgorithm;
  int _alias;
  int log;
}
class PrintCertCmd {
  int _certFileName;
  int log;
}
class Messages {
  int CACHED_FORMATS;
  int RESOURCE_BUNDLE;
  int BUNDLE_NAME;
  int log;
}
class Main {
  class ShutdownHook {
  }
  class NoParseOption {
  }
  int shutdownThread;
  int cmdLineParser;
  int gnuCallbacksNdx;
  int gnuCryptoProviderNdx;
  int helpPrinted;
  int X_509;
  int LAST_SERIAL_NUMBER;
  int DNAME_OPT;
  int RFC_OPT;
  int NEW_OPT;
  int DEST_OPT;
  int VERBOSE_OPT;
  int FILE_OPT;
  int PROVIDER_OPT;
  int KEYSTORE_OPT;
  int STOREPASS_OPT;
  int STORETYPE_OPT;
  int VALIDITY_OPT;
  int KEYPASS_OPT;
  int KEYSIZE_OPT;
  int KEYALG_OPT;
  int SIGALG_OPT;
  int ALIAS_OPT;
  int _CACERT;
  int _HELP;
  int _DELETE;
  int _KEYPASSWD;
  int _STOREPASSWD;
  int _KEYCLONE;
  int _PRINTCERT;
  int _LIST;
  int _EXPORT;
  int _CERTREQ;
  int _IDENTITYDB;
  int _SELFCERT;
  int _IMPORT;
  int _GENKEY;
  int CACERT_CMD;
  int DELETE_CMD;
  int KEYPASSWD_CMD;
  int STOREPASSWD_CMD;
  int KEYCLONE_CMD;
  int PRINTCERT_CMD;
  int LIST_CMD;
  int EXPORT_CMD;
  int CERTREQ_CMD;
  int IDENTITYDB_CMD;
  int SELFCERT_CMD;
  int IMPORT_CMD;
  int GENKEY_CMD;
  int KEYTOOL_TOOL;
  int log;
}
class ListCmd {
  int all;
  int rfc;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _alias;
  int log;
}
class KeyPasswdCmd {
  int newPasswordChars;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _newPassword;
  int _password;
  int _alias;
  int log;
}
class KeyCloneCmd {
  int newKeyPasswordChars;
  int destinationAlias;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _newPassword;
  int _password;
  int _destAlias;
  int _alias;
  int log;
}
class ImportCmd {
  int selfSignedCertificate;
  int jksCaCertsPathName;
  int gkrCaCertsPathName;
  int x509Factory;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int trustCACerts;
  int noPrompt;
  int _password;
  int _certFileName;
  int _alias;
  int CACERTS_GKR;
  int CACERTS;
  int SECURITY;
  int LIB;
  int JKS;
  int GKR;
  int log;
}
class IdentityDBCmd {
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _idbFileName;
  int log;
}
class GenKeyCmd {
  int distinguishedName;
  int keySize;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _validityStr;
  int _password;
  int _dName;
  int _sigAlgorithm;
  int _keySizeStr;
  int _keyAlgorithm;
  int _alias;
  int DEFAULT_KEY_SIZE;
  int log;
}
class ExportCmd {
  int rfc;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _certFileName;
  int _alias;
  int log;
}
class DeleteCmd {
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _alias;
  int log;
}
class Command {
  class ShutdownHook {
  }
  int shutdownThread;
  int handler;
  int providerNdx;
  int sha;
  int md5;
  int verbose;
  int inStream;
  int validityInDays;
  int signatureAlgorithm;
  int keyPairGenerator;
  int systemOut;
  int outStream;
  int store;
  int storeStream;
  int storeURL;
  int storePasswordChars;
  int storeType;
  int provider;
  int keyPasswordChars;
  int alias;
  int MILLIS_IN_A_DAY;
  int SHA1_WITH_RSA;
  int MD5_WITH_RSA;
  int MD2_WITH_RSA;
  int SHA1_WITH_DSA;
  int DEFAULT_VALIDITY;
  int RSA_SIGNATURE_ALGORITHM;
  int DSA_SIGNATURE_ALGORITHM;
  int DEFAULT_KEY_ALGORITHM;
  int DEFAULT_ALIAS;
  int log;
}
class CertReqCmd {
  int nullAttributes;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _password;
  int _certReqFileName;
  int _sigAlgorithm;
  int _alias;
  int ATTRIBUTES_OPT;
  int log;
}
class CACertCmd {
  int x509Factory;
  int _providerClassName;
  int _ksPassword;
  int _ksURL;
  int _ksType;
  int _certFileName;
  int log;
}
