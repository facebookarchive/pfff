package gnu.classpath.tools.taglets;
class VersionTaglet {
  int enabled;
  int HEADER;
  int NAME;
}
class ValueTaglet {
  int NAME;
}
class TagletContext {
  int doc;
}
class SinceTaglet {
  int inlineTagRenderer;
  int enabled;
  int HEADER;
  int NAME;
}
class GnuExtendedTaglet {
}
class GenericTaglet {
  int enabled;
  int scopeField;
  int scopeMethod;
  int scopeConstructor;
  int scopeType;
  int scopePackage;
  int scopeOverview;
  int header;
  int name;
}
class DeprecatedTaglet {
  int enabled;
  int HEADER;
  int NAME;
}
class CopyrightTaglet {
  int HEADER;
  int NAME;
}
class CodeTaglet {
  int NAME;
}
class AuthorTaglet {
  int authorEmailPattern;
  int atPattern;
  int dotPattern;
  int enabled;
  int MULTI_HEADER;
  int SINGLE_HEADER;
  int NAME;
  int dotReplacement;
  int atReplacement;
  int emailReplacementType;
  class EmailReplacement {
    int NAME_MANGLED_ADDRESS;
    int NAME_MAILTO_ADDRESS;
    int MAILTO_NAME;
    int NO_REPLACEMENT;
  }
}
