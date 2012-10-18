package com.android.webviewtests;
class WebViewStubActivity {
  int mWebView;
}
class JavaBridgeTestBase {
  int mWebViewClient;
  class Controller {
    int mIsResultReady;
  }
  class TestWebViewClient {
    int mIsPageFinished;
  }
}
class JavaBridgeReturnValuesTest {
  int mTestObject;
  class CustomType {
  }
  class TestObject {
    int mBooleanValue;
    int mStringValue;
  }
}
class JavaBridgeFieldsTest {
  int mTestObject;
  class CustomType {
  }
  class TestObject {
    int customTypeField;
    int objectField;
    int stringField;
    int doubleField;
    int floatField;
    int longField;
    int intField;
    int shortField;
    int charField;
    int byteField;
    int booleanField;
    int mStringValue;
  }
}
class JavaBridgeCoercionTest {
  int mTestObject;
  class CustomType2 {
  }
  class CustomType {
  }
  class TestObject {
    int mCustomTypeValue;
    int mObjectValue;
    int mStringValue;
    int mDoubleValue;
    int mFloatValue;
    int mLongValue;
    int mIntValue;
    int mShortValue;
    int mCharValue;
    int mByteValue;
    int mBooleanValue;
    int customType2Instance;
    int customTypeInstance;
    int objectInstance;
  }
}
class JavaBridgeBasicsTest {
  int mTestController;
  class ObjectWithStaticMethod {
  }
  class TestController {
    int mBooleanValue;
    int mStringValue;
    int mLongValue;
    int mIntValue;
  }
}
class JavaBridgeArrayTest {
  int mTestObject;
  class TestObject {
    int mWasArrayMethodCalled;
    int mIntIntArray;
    int mIntArray;
    int mStringValue;
    int mIntValue;
    int mBooleanValue;
  }
}
class JavaBridgeArrayCoercionTest {
  int mTestObject;
  class CustomType {
  }
  class TestObject {
    int mCustomTypeArray;
    int mObjectArray;
    int mStringArray;
    int mDoubleArray;
    int mFloatArray;
    int mLongArray;
    int mIntArray;
    int mShortArray;
    int mCharArray;
    int mByteArray;
    int mBooleanArray;
    int mCustomTypeInstance;
    int mObjectInstance;
  }
}
