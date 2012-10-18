package com.android.vcard.tests;
class VCardUtilsTests {
}
class VCardTestUtilsTests {
}
class VCardTestRunner {
}
class VCardParserTests {
  class MockVCardInterpreter {
    int mExpectedOrder;
    int mHistory;
  }
  class Order {
    int PROPERTY_CREATED;
    int END_ENTRY;
    int START_ENTRY;
    int END;
    int START;
  }
}
class VCardJapanizationTests {
}
class VCardImporterTests {
  int sPhotoByteArrayForComplicatedCase;
  int sPhotoIntArrayForComplicatedCase;
}
class VCardImporterNestTests {
}
class VCardExporterTests {
  int sPhotoByteArray;
}
class VCardEntryTests {
  class MockEntryElementIterator {
    int mExpectedElements;
    int mLabel;
    int mEndCalled;
    int mStartCalled;
  }
  class MockVCardEntryHandler {
    int mOnEndCalled;
    int mOnStartCalled;
    int mEntries;
  }
}
