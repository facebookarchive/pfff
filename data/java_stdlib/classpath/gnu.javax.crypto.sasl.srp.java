package gnu.javax.crypto.sasl.srp;
class StoreEntry {
  int timeToDie;
  int perenial;
}
class ServerStore {
  int counter;
  int lock;
  int sid2ttl;
  int sid2ssc;
  int singleton;
}
class SecurityContext {
  int outCipher;
  int inCipher;
  int outMac;
  int inMac;
  int outCounter;
  int inCounter;
  int replayDetection;
  int sIV;
  int cIV;
  int K;
  int sid;
  int mdName;
}
class SRPServer {
  int prng;
  int serverHandler;
  int outCipher;
  int inCipher;
  int outMac;
  int inMac;
  int outCounter;
  int inCounter;
  int replayDetection;
  int K;
  int rawSendSize;
  int chosenConfidentialityAlgorithm;
  int chosenIntegrityAlgorithm;
  int o;
  int L;
  int mandatory;
  int cCB;
  int ttl;
  int sid;
  int srp;
  int sn;
  int cn;
  int sIV;
  int cIV;
  int s;
  int B;
  int A;
  int g;
  int N;
  int U;
  int log;
}
class SRPRegistry {
  int MINIMUM_MODULUS_BITLENGTH;
  int CONFIG_NDX_FIELD;
  int SALT_FIELD;
  int USER_VERIFIER_FIELD;
  int MD_NAME_FIELD;
  int DEFAULT_MANDATORY;
  int MANDATORY_NONE;
  int OPTION_MAX_BUFFER_SIZE;
  int OPTION_CONFIDENTIALITY;
  int OPTION_INTEGRITY;
  int OPTION_REPLAY_DETECTION;
  int OPTION_SRP_DIGEST;
  int OPTION_MANDATORY;
  int CONFIDENTIALITY_ALGORITHMS;
  int BLOWFISH;
  int AES;
  int INTEGRITY_ALGORITHMS;
  int HMAC_RIPEMD_160;
  int HMAC_MD5;
  int HMAC_SHA1;
  int DEFAULT_CONFIDENTIALITY;
  int DEFAULT_INTEGRITY;
  int DEFAULT_REPLAY_DETECTION;
  int DEFAULT_PASSWORD_FILE;
  int PASSWORD_DB;
  int PASSWORD_FILE;
  int SRP_CONFIDENTIALITY;
  int SRP_INTEGRITY_PROTECTION;
  int SRP_REPLAY_DETECTION;
  int SRP_MANDATORY;
  int SRP_HASH;
  int SERVER_EVIDENCE;
  int CLIENT_EVIDENCE;
  int SERVER_PUBLIC_KEY;
  int CLIENT_PUBLIC_KEY;
  int PASSWORD_VERIFIER;
  int USER_SALT;
  int USER_ROLE;
  int USER_NAME;
  int CHOSEN_OPTIONS;
  int AVAILABLE_OPTIONS;
  int FIELD_GENERATOR;
  int SHARED_MODULUS;
  int SRP_DIGEST_NAME;
  int SRP_DEFAULT_DIGEST_NAME;
  int SRP_ALGORITHMS;
  int N_512_BITS;
  int N_640_BITS;
  int N_768_BITS;
  int N_1024_BITS;
  int N_1280_BITS;
  int N_1536_BITS;
  int N_2048_BITS;
}
class SRPClient {
  int prng;
  int clientHandler;
  int outCipher;
  int inCipher;
  int outMac;
  int inMac;
  int outCounter;
  int inCounter;
  int replayDetection;
  int K;
  int rawSendSize;
  int chosenConfidentialityAlgorithm;
  int chosenIntegrityAlgorithm;
  int o;
  int L;
  int sCB;
  int ttl;
  int sid;
  int srp;
  int sn;
  int cn;
  int M2;
  int M1;
  int sIV;
  int cIV;
  int s;
  int password;
  int B;
  int A;
  int g;
  int N;
  int U;
  int uid;
  int log;
}
class SRPAuthInfoProvider {
  int passwordFile;
}
class SRP {
  int mda;
  int COLON;
  int algorithms;
}
class PasswordFile {
  int Nsrp;
  int configurations;
  int entries;
  int lastmodPasswd2File;
  int lastmodPasswdFile;
  int passwd2File;
  int passwdFile;
  int configFile;
  int pw2Name;
  int pwName;
  int confName;
  int srps;
  int DEFAULT_FILE;
  int CONFIG_FIELD;
  int SALT_FIELD;
  int VERIFIERS_FIELD;
  int USER_FIELD;
}
class KDF {
  int umac;
  int prng;
  int buffer;
  int AES_KEY_SIZE;
  int AES_BLOCK_SIZE;
}
class IALG {
  int hmac;
}
class ClientStore {
  int lock;
  int uid2ttl;
  int uid2ssc;
  int singleton;
}
class CALG {
  int keySize;
  int blockSize;
  int modeNdx;
  int assembly;
}
