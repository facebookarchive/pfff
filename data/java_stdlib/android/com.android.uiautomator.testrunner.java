package com.android.uiautomator.testrunner;
class UiAutomatorTestRunner {
  class StringResultPrinter {
  }
  class WatcherResultPrinter {
    int mTestClass;
    int mTestResultCode;
    int mTestNum;
    int mTestResult;
    int mResultTemplate;
    int REPORT_VALUE_RESULT_FAILURE;
    int REPORT_VALUE_RESULT_ERROR;
    int REPORT_VALUE_RESULT_START;
    int REPORT_KEY_STACK;
    int REPORT_VALUE_ID;
    int REPORT_KEY_NUM_ITERATIONS;
    int REPORT_KEY_NAME_TEST;
    int REPORT_KEY_NUM_CURRENT;
    int REPORT_KEY_NAME_CLASS;
    int REPORT_KEY_NUM_TOTAL;
  }
  class FakeInstrumentationWatcher {
    int mRawMode;
  }
  int mTestListeners;
  int mAutomationSupport;
  int mWatcher;
  int mTestClasses;
  int mUiDevice;
  int mParams;
  int mDebug;
  int EXIT_EXCEPTION;
  int EXIT_OK;
  int LOGTAG;
}
class UiAutomatorTestCaseFilter {
}
class UiAutomatorTestCase {
  int mShouldDisableIme;
  int mAutomationSupport;
  int mParams;
  int mUiDevice;
  int DUMMY_IME_PACKAGE;
  int DISABLE_IME;
}
class TestCaseCollector {
  class TestCaseFilter {
  }
  int mFilter;
  int mTestCases;
  int mClassLoader;
}
class IAutomationSupport {
}
