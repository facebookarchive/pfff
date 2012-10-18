package gnu.classpath.tools.doclets.xmldoclet;
class TargetContext {
  int xsltTargetDirectory;
  int targetDirectory;
  int docTranslet;
}
class HtmlRepairer {
  int noTextParentTags;
  int tagInfoMap;
  int throwAwayLeadingPara;
  int isLeadingTag;
  int tagStack;
  int output;
  int contextMember;
  int contextClass;
  int noEmailWarn;
  int noWarn;
  int warningReporter;
  class TagInfo {
    int parentTags;
  }
}
class Driver1_4 {
}
class Driver {
  int usedClassToPackagesMap;
  class UsageType {
    int id;
    int CONSTRUCTOR_WITH_THROWN_TYPE;
    int CONSTRUCTOR_WITH_PARAMETER_TYPE;
    int METHOD_WITH_THROWN_TYPE;
    int METHOD_WITH_PARAMETER_TYPE;
    int METHOD_WITH_RETURN_TYPE;
    int FIELD_OF_TYPE;
    int CLASS_DERIVED_FROM;
  }
  int htmlRepairer;
  int packageGroups;
  int docTransletOptions;
  int mentionedTags;
  int tagletMap;
  int currentExecMember;
  int currentMember;
  int currentClass;
  int tagletPath;
  int tagPrefix;
  int rootDoc;
  int excludeDocFilesSubDirs;
  int docFilesSubdirsEnabled;
  int workingDirectory;
  int workingPath;
  int title;
  int bottomNote;
  int targets;
  int xmlTargetDirectory;
  int targetDirectory;
  int fixHTML;
  int noEmailWarn;
  int noHTMLWarn;
  int compress;
  int indentStep;
  int out;
  int CONTEXT_TYPE;
  int CONTEXT_PACKAGE;
  int CONTEXT_OVERVIEW;
  int CONTEXT_METHOD;
  int CONTEXT_FIELD;
  int CONTEXT_CONSTRUCTOR;
  class NullErrorReporter {
  }
  int XMLDOCLET_VERSION;
}
