package gnu.classpath.tools.getopt;
class Parser {
  int args;
  int currentIndex;
  int finalGroup;
  int defaultGroup;
  int optionGroups;
  int options;
  int longOnly;
  int footerText;
  int headerText;
  int programName;
  int MAX_LINE_LENGTH;
}
class OptionGroup {
  int options;
  int name;
  int FILLER;
}
class OptionException {
}
class Option {
  int joined;
  int argumentName;
  int description;
  int longName;
  int shortName;
}
class Messages {
  int RESOURCE_BUNDLE;
  int BUNDLE_NAME;
}
class FileArgumentCallback {
}
