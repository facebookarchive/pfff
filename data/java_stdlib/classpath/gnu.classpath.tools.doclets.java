package gnu.classpath.tools.doclets;
class TagletPrinter {
}
class StandardTaglet {
  int name;
}
class PackageMatcher {
  int patterns;
}
class PackageGroup {
  int packages;
  int name;
}
class InvalidPackageWildcardException {
}
class InlineTagRenderer {
}
class DocletOptionString {
  int value;
}
class DocletOptionPackageWildcard {
  int specified;
  int allowAll;
  int packageMatcher;
}
class DocletOptionFlag {
  int value;
}
class DocletOptionFile {
  int value;
}
class DocletOptionColonSeparated {
  int components;
}
class DocletOption {
  int optionName;
}
class DocletConfigurationException {
}
class AbstractDoclet {
  int implementedInterfacesCache;
  int sortedInnerClassMap;
  int sortedFieldMap;
  int sortedConstructorMap;
  int sortedMethodMap;
  int interfaceRelations;
  int allSubClasses;
  class InterfaceRelation {
    int implementingClasses;
    int subInterfaces;
    int superInterfaces;
  }
  int allPackages;
  int sourcePaths;
  int resources;
  class UsageType {
    int id;
    int CONSTRUCTOR_WITH_THROWN_TYPE;
    int CONSTRUCTOR_WITH_PARAMETER_TYPE;
    int METHOD_WITH_THROWN_TYPE;
    int METHOD_WITH_PARAMETER_TYPE;
    int METHOD_WITH_RETURN_TYPE;
    int FIELD_OF_TYPE;
    int CLASS_IMPLEMENTING;
    int CLASS_DERIVED_FROM;
  }
  int usedClassToPackagesMap;
  int indexByName;
  int categorizedIndex;
  class IndexKey {
    int lowerName;
    int name;
  }
  int nameToOptionMap;
  int optionsRegistered;
  int commonOptions;
  class DocletOptionTag {
  }
  class DocletOptionGroup {
  }
  int optionTag;
  int optionTaglet;
  int optionExcludeDocFilesSubDir;
  int optionDocFilesSubDirs;
  int optionNoQualifier;
  int optionGroup;
  int optionNoDeprecated;
  int optionNoSince;
  int optionVersion;
  int optionAuthor;
  int optionTargetDirectory;
  int rootDoc;
  int instance;
  int mentionedTags;
  int packageGroups;
  int tagletMap;
}
