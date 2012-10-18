package android.test.suitebuilder;
class UnitTestSuiteBuilderTest {
  class SuiteExecutionRecorder {
    int testsSeen;
    int errors;
    int failures;
  }
  int unitTestSuiteBuilder;
}
class UnitTestSuiteBuilder {
}
class TestSuiteBuilderTest {
  class SuiteExecutionRecorder {
    int testsSeen;
    int errors;
    int failures;
  }
  int testSuiteBuilder;
}
class TestSuiteBuilder {
  class FailedToCreateTests {
    int exception;
  }
  int suiteName;
  int currentClassname;
  int suiteForCurrentClass;
  int rootSuite;
  int testCases;
  int predicates;
  int testGrouping;
  int context;
}
class TestPredicates {
  int REJECT_SUPPRESSED;
  int SELECT_LARGE;
  int SELECT_MEDIUM;
  int SELECT_SMALL;
  int SELECT_SMOKE;
  int REJECT_INSTRUMENTATION;
  int SELECT_INSTRUMENTATION;
}
class TestMethod {
  int enclosingClass;
  int testMethodName;
  int enclosingClassname;
}
class TestGroupingTest {
  int mGrouping;
}
class TestGrouping {
  class TestMethodPredicate {
  }
  class TestCasePredicate {
  }
  class SortByFullyQualifiedName {
  }
  class SortBySimpleName {
  }
  int classLoader;
  int firstIncludedPackage;
  int SORT_BY_FULLY_QUALIFIED_NAME;
  int SORT_BY_SIMPLE_NAME;
  int testCaseClasses;
  int LOG_TAG;
}
class SmokeTestSuiteBuilderTest {
}
class SmokeTestSuiteBuilder {
}
class ListTestCaseNames {
  class TestDescriptor {
    int mTestName;
    int mClassName;
  }
}
class InstrumentationTestSuiteBuilderTest {
  class SuiteExecutionRecorder {
    int testsSeen;
    int errors;
    int failures;
  }
  int instrumentationTestSuiteBuilder;
}
class InstrumentationTestSuiteBuilder {
}
class AssignableFromTest {
  class Pen {
  }
  class Pencil {
  }
  class WritingInstrument {
  }
  class Human {
  }
  class Mammal {
  }
  class Animal {
  }
  int assignableFrom;
}
class AssignableFrom {
  int root;
}
