package java.util.jar;
class Pack200 {
  class Unpacker {
    int TRUE;
    int PROGRESS;
    int KEEP;
    int FALSE;
    int DEFLATE_HINT;
  }
  class Packer {
    int UNKNOWN_ATTRIBUTE;
    int TRUE;
    int STRIP;
    int SEGMENT_LIMIT;
    int PROGRESS;
    int PASS_FILE_PFX;
    int PASS;
    int MODIFICATION_TIME;
    int METHOD_ATTRIBUTE_PFX;
    int LATEST;
    int KEEP_FILE_ORDER;
    int KEEP;
    int FIELD_ATTRIBUTE_PFX;
    int FALSE;
    int ERROR;
    int EFFORT;
    int DEFLATE_HINT;
    int CODE_ATTRIBUTE_PFX;
    int CLASS_ATTRIBUTE_PFX;
  }
  int SYSTEM_PROPERTY_UNPACKER;
  int SYSTEM_PROPERTY_PACKER;
}
class Manifest {
  int mainEnd;
  int chunks;
  class Chunk {
    int end;
    int start;
  }
  int entries;
  int mainAttributes;
  int BAIS_POS;
  int BAIS_BUF;
  int NAME_ATTRIBUTE;
  int VALUE_SEPARATOR;
  int LINE_SEPARATOR;
  int LINE_LENGTH_LIMIT;
}
class JarVerifier {
  class VerifierEntry {
    int certificates;
    int hash;
    int digest;
    int name;
  }
  int mainAttributesEnd;
  int verifiedEntries;
  int certificates;
  int signatures;
  int metaEntries;
  int man;
  int jarName;
}
class JarOutputStream {
  int manifest;
}
class JarInputStream {
  int verStream;
  int verifier;
  int isMeta;
  int jarEntry;
  int mEntry;
  int eos;
  int manifest;
}
class JarFile {
  class JarFileInputStream {
    int done;
    int entry;
    int zipEntry;
    int count;
  }
  int closed;
  int verifier;
  int manifestEntry;
  int manifest;
  int META_DIR;
  int MANIFEST_NAME;
}
class JarException {
  int serialVersionUID;
}
class JarEntry {
  int isFactoryChecked;
  int factory;
  int signers;
  int parentJar;
  int attributes;
}
class InitManifest {
  int consecutiveLineBreaks;
  int valueBuffer;
  int value;
  int name;
  int pos;
  int buf;
}
class Attributes {
  class Name {
    int NAME;
    int IMPLEMENTATION_URL;
    int IMPLEMENTATION_VENDOR_ID;
    int EXTENSION_INSTALLATION;
    int EXTENSION_NAME;
    int EXTENSION_LIST;
    int SPECIFICATION_VENDOR;
    int SPECIFICATION_VERSION;
    int SPECIFICATION_TITLE;
    int IMPLEMENTATION_VENDOR;
    int IMPLEMENTATION_VERSION;
    int IMPLEMENTATION_TITLE;
    int SEALED;
    int CONTENT_TYPE;
    int SIGNATURE_VERSION;
    int MAIN_CLASS;
    int MANIFEST_VERSION;
    int CLASS_PATH;
    int name;
  }
  int map;
}
