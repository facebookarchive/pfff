package com.android.vcard.tests.testutils;
class VNodeBuilder {
  int mStrictLineBreakParsing;
  int mTargetCharset;
  int mSourceCharset;
  int mCurrentVNode;
  int mVNodeList;
  int LOG_TAG;
}
class VNode {
  int propList;
}
class VCardVerifier {
  int mCharset;
  int mVerified;
  int mInitialized;
  int mContentValuesVerifier;
  int mLineVerifier;
  int mPropertyNodesVerifier;
  int mInputStream;
  int mExportTestResolver;
  int mIsDoCoMo;
  int mVCardType;
  int mAndroidTestCase;
  class CustomMockContext {
    int mResolver;
  }
  int CONTACTS_TEST_CONTENT_URI;
  int VCARD_TEST_AUTHORITY_URI;
  int VCARD_TEST_AUTHORITY;
  int DEBUG;
  int LOG_TAG;
}
class VCardTestsBase {
  int mSkipVerification;
  int mVerifier;
  int mContentValuesForBase64V30;
  int mContentValuesForBase64V21;
  int mContentValuesForQPAndUtf8;
  int mContentValuesForQPAndSJis;
  int mContentValuesForUtf8;
  int mContentValuesForSJis;
  int mContentValuesForQP;
  int V40;
  int V30;
  int V21;
}
class PropertyNodesVerifierElem {
  int mUnorderedNodeList;
  int mOrderedNodeMap;
  class GroupSet {
  }
  class TypeSet {
  }
}
class PropertyNodesVerifier {
  int mIndex;
  int mAndroidTestCase;
  int mPropertyNodesVerifierElemList;
}
class PropertyNode {
  int propGroupSet;
  int paramMap_TYPE;
  int paramMap;
  int propValue_bytes;
  int propValue_vector;
  int propValue;
  int propName;
}
class LineVerifierElem {
  int mVCardType;
  int mExpectedLineList;
}
class LineVerifier {
  int index;
  int mVCardType;
  int mLineVerifierElemList;
  int mAndroidTestCase;
}
class ImportTestResolver {
  int mProvider;
}
class ImportTestProvider {
  int mMimeTypeToExpectedContentValues;
}
class ExportTestResolver {
  int mProvider;
}
class ExportTestProvider {
  class MockEntityIterator {
    int mIterator;
    int mEntityList;
  }
  int mContactEntryList;
}
class ContentValuesVerifierElem {
  int mHandler;
  int mResolver;
}
class ContentValuesVerifier {
  int mIndex;
  int mContentValuesVerifierElemList;
}
class ContentValuesBuilder {
  int mContentValues;
}
class ContactEntry {
  int mContentValuesList;
}
