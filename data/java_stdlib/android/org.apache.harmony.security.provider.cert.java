package org.apache.harmony.security.provider.cert;
class X509CertPathImpl {
  int PKCS7_SIGNED_DATA_OBJECT;
  int ASN1_SIGNED_DATA;
  int ASN1;
  int pkcs7Encoding;
  int pkiPathEncoding;
  int certificates;
  int encodings;
  int encodingsArr;
  int PKCS7;
  int PKI_PATH;
  int serialVersionUID;
}
class X509CertImpl {
  int encoding;
  int publicKey;
  int nullSigAlgParams;
  int sigAlgParams;
  int sigAlgOID;
  int sigAlgName;
  int signature;
  int tbsCertificate;
  int subject;
  int issuer;
  int serialNumber;
  int notAfter;
  int notBefore;
  int extensions;
  int tbsCert;
  int certificate;
  int serialVersionUID;
}
class X509CertFactoryImpl {
  class RestoringInputStream {
    int end;
    int bar;
    int pos;
    int buff;
    int BUFF_SIZE;
    int inStream;
  }
  int CERT_BOUND_SUFFIX;
  int FREE_BOUND_SUFFIX;
  int PEM_END;
  int PEM_BEGIN;
  int CRL_CACHE;
  int CRL_CACHE_SEED_LENGTH;
  int CERT_CACHE;
  int CERT_CACHE_SEED_LENGTH;
}
class X509CRLImpl {
  int nonIndirectEntriesSize;
  int isIndirectCRL;
  int entriesRetrieved;
  int nullSigAlgParams;
  int encoding;
  int sigAlgParams;
  int sigAlgName;
  int sigAlgOID;
  int signature;
  int entriesSize;
  int entries;
  int issuer;
  int extensions;
  int tbsCertListEncoding;
  int tbsCertList;
  int crl;
}
class X509CRLEntryImpl {
  int encoding;
  int issuer;
  int extensions;
  int rcert;
}
class DRLCertFactory {
  int serialVersionUID;
}
class Cache {
  int cache_is_full;
  int last_cached;
  int hashes_idx;
  int cache;
  int encodings;
  int hashes;
  int prefix_size;
  int cache_size;
  int INDEX_MASK;
  int PREFIX_HASH_MASK;
  int HASH_MASK;
}
