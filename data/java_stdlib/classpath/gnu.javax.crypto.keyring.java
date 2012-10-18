package gnu.javax.crypto.keyring;
class PublicKeyEntry {
  int key;
  int TYPE;
}
class Properties {
  int props;
}
class PrivateKeyEntry {
  int key;
  int TYPE;
}
class PrimitiveEntry {
  int creationDate;
}
class PasswordProtectedEntry {
  int ITERATION_COUNT;
}
class PasswordEncryptedEntry {
  int TYPE;
  int log;
}
class PasswordAuthenticatedEntry {
  int TYPE;
  int log;
}
class MeteredInputStream {
  int limit;
  int count;
}
class MaskableEnvelopeEntry {
  int masked;
}
class MalformedKeyringException {
}
class IPublicKeyring {
}
class IPrivateKeyring {
}
class IKeyring {
  int KEYRING_PASSWORD;
  int KEYRING_DATA_OUT;
  int KEYRING_DATA_IN;
}
class GnuPublicKeyring {
  int USAGE;
  int log;
}
class GnuPrivateKeyring {
  int keylen;
  int mode;
  int cipher;
  int maclen;
  int mac;
  int USAGE;
  int log;
}
class EnvelopeEntry {
  int entries;
  int containingEnvelope;
  int log;
}
class Entry {
  int payload;
  int properties;
  int type;
  int TYPES;
  int log;
}
class EncryptedEntry {
  int TYPE;
}
class CompressedEntry {
  int TYPE;
}
class CertificateEntry {
  int certificate;
  int TYPE;
}
class CertPathEntry {
  int path;
  int TYPE;
}
class BinaryDataEntry {
  int TYPE;
}
class BaseKeyring {
  int keyring2;
  int keyring;
}
class AuthenticatedEntry {
  int TYPE;
}
