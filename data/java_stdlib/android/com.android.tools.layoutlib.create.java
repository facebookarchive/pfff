package com.android.tools.layoutlib.create;
class TransformClassAdapter {
  int mDeleteReturns;
  int mStubMethods;
  int mLog;
  int mClassName;
  int mIsInterface;
  int mStubAll;
}
class StubMethodAdapter {
  int mIsNative;
  int mIsStatic;
  int mMessageGenerated;
  int mIsInitMethod;
  int mOutputFirstLineNumber;
  int mInvokeSignature;
  int mReturnType;
  int mParentVisitor;
  int CLASS_INIT;
  int CONSTRUCTOR;
}
class RenameClassAdapterTest {
  int mInner;
  int mOuter;
}
class RenameClassAdapter {
  class RenameSignatureAdapter {
    int mSv;
  }
  class RenameMethodAdapter {
  }
  int mNewBase;
  int mOldBase;
  int mNewName;
  int mOldName;
}
class OverrideMethod {
  int sDefaultListener;
  int sMethods;
}
class MockLog {
  int mErr;
  int mOut;
}
class MethodListener {
}
class MethodAdapter {
}
class Main {
  int sOptions;
  class Options {
    int generatePublicAccess;
  }
}
class LogTest {
  int mLog;
}
class LogAbortException {
  int mArgs;
  int mFormat;
}
class Log {
  int mVerbose;
}
class ICreateInfo {
}
class DelegateMethodAdapter2 {
  int mDelegateLineNumber;
  int mLog;
  int mMethodName;
  int mClassName;
  int mIsStatic;
  int mDesc;
  int mDelWriter;
  int mOrgWriter;
  int DELEGATE_SUFFIX;
}
class DelegateClassAdapterTest {
  class ClassLoader2 {
    int mClassDefs;
  }
  int INNER_CLASS_NAME;
  int OUTER_CLASS_NAME;
  int NATIVE_CLASS_NAME;
  int mLog;
}
class DelegateClassAdapter {
  int mLog;
  int mDelegateMethods;
  int mClassName;
  int ALL_NATIVES;
  int CLASS_INIT;
  int CONSTRUCTOR;
  int ORIGINAL_SUFFIX;
}
class CreateInfo {
  int DELETE_RETURNS;
  int RENAMED_CLASSES;
  int OVERRIDDEN_METHODS;
  int DELEGATE_CLASS_NATIVES;
  int DELEGATE_METHODS;
  int INJECTED_CLASSES;
}
class ClassHasNativeVisitorTest {
  class ClassWithoutNative {
  }
  class ClassWithNative {
  }
  class MockClassHasNativeVisitor {
    int mMethodsFound;
  }
}
class ClassHasNativeVisitor {
  int mHasNativeMethods;
}
class AsmGeneratorTest {
  int mTempFile;
  int mOsDestJar;
  int mOsJarPath;
  int mLog;
}
class AsmGenerator {
  int mDelegateMethods;
  int mDeleteReturns;
  int mClassesNotRenamed;
  int mRenameClasses;
  int mRenameCount;
  int mDeps;
  int mKeep;
  int mStubMethods;
  int mInjectClasses;
  int mOsDestJar;
  int mLog;
}
class AsmAnalyzerTest {
  int mAa;
  int mOsJarPath;
  int mLog;
}
class AsmAnalyzer {
  class DependencyVisitor {
    class MyAnnotationVisitor {
    }
    class MySignatureVisitor {
      int mCurrentSignatureClass;
    }
    class MyMethodVisitor {
    }
    class MyFieldVisitor {
    }
    int mOutKeep;
    int mOutDeps;
    int mInDeps;
    int mInKeep;
    int mZipClasses;
  }
  int mIncludeGlobs;
  int mDeriveFrom;
  int mGen;
  int mOsSourceJar;
  int mLog;
}
