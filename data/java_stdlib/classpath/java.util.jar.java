package java.util.jar;
class Manifest {
  int entries;
  int mainAttr;
}
class JarOutputStream {
}
class JarInputStream {
  int firstEntry;
  int manifest;
}
class JarFile {
  class EntryInputStream {
    int checked;
    int md;
    int hashes;
    int entry;
    int pos;
    int length;
    int jarfile;
  }
  class JarEnumeration {
    int jarfile;
    int entries;
  }
  int DEBUG;
  int digestAlgorithms;
  int entryCerts;
  int verified;
  int signaturesRead;
  int manifestRead;
  int verify;
  int manifest;
  int RSA_ENCRYPTION_OID;
  int DSA_ENCRYPTION_OID;
  int SHA1_OID;
  int MD5_OID;
  int MD4_OID;
  int MD2_OID;
  int provider;
  int SF_SUFFIX;
  int DIGEST_KEY_SUFFIX;
  int PKCS7_RSA_SUFFIX;
  int PKCS7_DSA_SUFFIX;
  int META_INF;
  int MANIFEST_NAME;
}
class JarException {
  int serialVersionUID;
}
class JarEntry {
  int jarfile;
  int attr;
}
class Attributes {
  class Name {
    int origName;
    int name;
    int CONTENT_TYPE;
    int SEALED;
    int SPECIFICATION_VENDOR;
    int SPECIFICATION_VERSION;
    int SPECIFICATION_TITLE;
    int IMPLEMENTATION_URL;
    int IMPLEMENTATION_VENDOR_ID;
    int IMPLEMENTATION_VENDOR;
    int IMPLEMENTATION_VERSION;
    int IMPLEMENTATION_TITLE;
    int EXTENSION_INSTALLATION;
    int EXTENSION_NAME;
    int EXTENSION_LIST;
    int MAIN_CLASS;
    int CLASS_PATH;
    int SIGNATURE_VERSION;
    int MANIFEST_VERSION;
  }
  int map;
}
