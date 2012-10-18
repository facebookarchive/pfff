package gnu.classpath.tools.gjdoc;
class WritableType {
}
class ValueTagImpl {
}
class TypeVariableImpl {
  int owner;
  int bounds;
}
class TypeImpl {
  int primitiveNames;
  int dimension;
  int typeName;
  int packageName;
}
class TimerDoclet {
  int runMemThread;
  int memThread;
  int maximumHeap;
  int doclet;
}
class Timer {
  int maxDocletHeap;
  int maxDriverHeap;
  int memoryUsed;
  int stopTime;
  int beforeDocletTime;
  int startTime;
}
class ThrowsTagImpl {
  int exceptionComment;
  int exceptionName;
  int exception;
}
class TextTagImpl {
}
class TemporaryStore {
  int storedObject;
}
class TagImpl {
  int name;
  int kind;
}
class TagContainer {
}
class SourcePositionImpl {
  int column;
  int line;
  int file;
}
class SerialFieldTagImpl {
  int contextClass;
  int fieldTypeDoc;
  int description;
  int fieldType;
  int fieldName;
}
class SeeTagImpl {
  int contextClass;
  int label;
  int referencedPackage;
  int referencedMember;
  int referencedClass;
  int referencedMemberName;
  int referencedClassName;
  int reference;
}
class RootDocImpl {
  int recursiveClasses;
  int unlocatableReflectedClassNames;
  int resolvedImportCache;
  int unlocatablePrefixes;
  class ResolvedImportReflectionPackage {
    int packagePrefix;
  }
  class ResolvedImportReflectionClass {
    int name;
    int clazz;
  }
  class ResolvedImportClassFile {
    int qualifiedName;
    int alreadyFetched;
    int classDoc;
    int name;
    int innerClassName;
    int classFile;
  }
  class ResolvedImportPackageFile {
    int cache;
    int packageName;
    int packageFile;
    int topLevelClassNames;
  }
  class ResolvedImportNotFound {
    int name;
    int importSpecifier;
  }
  class ResolvedImport {
  }
  class ScheduledClass {
    int qualifiedName;
    int contextClass;
  }
  int inaccessibleReportedSet;
  int unlocatableReportedSet;
  int parser;
  int sourceEncoding;
  int sourcePath;
  int scheduledClasses;
  int specifiedPackages;
  int specifiedClasses;
  int classes;
  int packageDocMap;
  int classDocMap;
  int classesList;
  int specifiedPackageNames;
  int specifiedSourceFiles;
  int customOptionArr;
  int rawCommentCache;
  int reporter;
}
class ProgramElementDocImpl {
  int accessModifiers;
  int ACCESS_PRIVATE;
  int ACCESS_PACKAGEPRIVATE;
  int ACCESS_PROTECTED;
  int ACCESS_PUBLIC;
  int accessLevel;
  int isStatic;
  int isFinal;
  int containingPackage;
  int containingClass;
}
class Parser {
  int boilerplateComment;
  int referencedClassesList;
  int importedStatementList;
  int importedPackagesList;
  int importedStringList;
  int importedClassesList;
  int interfacesList;
  int allClassesList;
  int ordinaryClassesList;
  int outerClass;
  int currentClass;
  int currentPackage;
  int currentPackageName;
  int expectedPackageName;
  int lastComment;
  int currentFile;
  class Context {
    int filteredInnerClassesList;
    int innerClassesList;
    int filteredConstructorList;
    int constructorList;
    int maybeSerMethodList;
    int filteredMethodList;
    int methodList;
    int sfieldList;
    int filteredFieldList;
    int fieldList;
    int classDoc;
  }
  int contextStack;
  int ctx;
  int processedFiles;
  int classLevelComponents;
  int sourceLevelComponents;
  int currentLine;
  int WHITESPACE;
  int addComments;
}
class ClassComponent {
}
class StaticBlockComponent {
}
class FunctionComponent {
}
class FieldComponent {
}
class PackageComponent {
}
class ImportComponent {
}
class EmptyStatementComponent {
}
class SlashSlashCommentComponent {
}
class CommentComponent {
}
class BracketClose {
}
class Whitespace {
}
class BlockSourceComponent {
}
class SourceComponent {
}
class IgnoredFileParseException {
}
class ParseException {
}
class ParameterImpl {
  int type;
  int typeName;
  int name;
}
class ParamTagImpl {
  int parameterComment;
  int parameterName;
}
class PackageDocImpl {
  int isIncluded;
  int DEFAULT_PACKAGE;
  int errors;
  int interfaces;
  int exceptions;
  int ordinaryClasses;
  int allClasses;
  int errorsList;
  int interfacesList;
  int exceptionsList;
  int ordinaryClassesList;
  int allClassesSet;
  int packageDirectory;
  int packageName;
}
class MethodDocImpl {
}
class MemberDocImpl {
  int name;
  int type;
  int typeName;
}
class Main {
  int options;
  class OptionProcessor {
    int argCount;
  }
  int option_reflection;
  int option_all;
  int option_bootclasspath_specified;
  int option_showVersion;
  int collator;
  int option_licensetext;
  int option_breakiterator;
  int option_exclude;
  int option_subpackages;
  int option_source;
  int option_encoding;
  int option_locale;
  int option_sourcepath;
  int option_docletpath;
  int option_help;
  int option_coverage;
  int option_doclet;
  int docletRunning;
  int gjdocVersion;
  int reporter;
  int rootDoc;
  int instance;
  int coverageTemplates;
  int STRING_TRY_GJDOC_HELP;
  int COVERAGE_PRIVATE;
  int COVERAGE_PACKAGE;
  int COVERAGE_PROTECTED;
  int COVERAGE_PUBLIC;
  int DESCEND_IMPORTED;
  int DESCEND_INTERFACES;
  int DESCEND_SUPERCLASS;
}
class LinkTagImpl {
  int name;
}
class JavadocWrapper {
}
class InheritDocTagImpl {
  int inheritedDocInitialized;
  int inheritedDoc;
  int contextTag;
  int contextMember;
  int contextClass;
}
class GjdocRootDoc {
}
class GjdocPackageDoc {
}
class FieldDocImpl {
  int constantValueEvaluated;
  int constantValue;
  int valueLiteral;
  int isVolatile;
  int isTransient;
}
class ExecutableMemberDocImpl {
  int flatSignature;
  int signature;
  int parameters;
  int thrownExceptions;
  int isSynchronized;
  int isNative;
  int isAbstract;
}
class ErrorReporter {
  int noWarn;
  int quiet;
  int warningCount;
  int errorCount;
  int out;
}
class DocImpl {
  int classDocToFileMap;
  int tagMap;
  int rawDocOffset;
  int rawDocumentation;
  int bufPos;
  int charBuf;
  int whitespaceBuf;
  int boilerplateComment;
  int position;
  int throwsTagEmptyArr;
  int paramTagEmptyArr;
  int linkTagEmptyArr;
  int seeTagEmptyArr;
}
class DirectoryTree {
  int root;
  class FileNode {
    int subNodes;
    int file;
  }
}
class Debug {
  int logLevel;
}
class ConstructorDocImpl {
}
class ClassDocReflectedImpl {
  int reflectionCache;
  int dimension;
  int unfilteredInnerClasses;
  int superclassDoc;
  int name;
  int clazz;
}
class ClassDocProxy {
  int dimension;
  int classContext;
  int qualifiedName;
  int name;
}
class ClassDocImpl {
  int importStatementList;
  int maybeSerMethodList;
  int typeMap;
  int isIncluded;
  int superclass;
  int resolved;
  int typeParameters;
  int unfilteredConstructors;
  int filteredConstructors;
  int unfilteredMethods;
  int filteredMethods;
  int serializableFields;
  int unfilteredFields;
  int filteredFields;
  int unfilteredInnerClasses;
  int filteredInnerClasses;
  int interfaces;
  int isEnum;
  int isAnnotation;
  int isInterface;
  int isAbstract;
  int className;
  int findClassCache;
  int primitiveNames;
  int dimension;
  int serializationMethods;
  int serialPersistentField;
  int definesSerializableFields;
  int importedPackages;
  int importedClasses;
  int baseClassDoc;
}
class ArrayCharacterIterator {
  int currentIndex;
  int endIndex;
  int beginIndex;
  int data;
}
class AbstractTagImpl {
  int tagMap;
  int text;
  int emptyTags;
}
