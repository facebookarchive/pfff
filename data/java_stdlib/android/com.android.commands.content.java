package com.android.commands.content;
class Content {
  class UpdateCommand {
    int mWhere;
  }
  class QueryCommand {
    int mSortOrder;
    int mProjection;
  }
  class DeleteCommand {
    int mWhere;
  }
  class InsertCommand {
    int mContentValues;
  }
  class Command {
    int mUri;
  }
  class Tokenizer {
    int mNextArg;
    int mArgs;
  }
  class Parser {
    int mTokenizer;
    int ARGUMENT_PREFIX;
    int COLON;
    int TYPE_DOUBLE;
    int TYPE_FLOAT;
    int TYPE_LONG;
    int TYPE_INTEGER;
    int TYPE_STRING;
    int TYPE_BOOLEAN;
    int ARGUMENT_SORT;
    int ARGUMENT_PROJECTION;
    int ARGUMENT_URI;
    int ARGUMENT_BIND;
    int ARGUMENT_WHERE;
    int ARGUMENT_QUERY;
    int ARGUMENT_UPDATE;
    int ARGUMENT_DELETE;
    int ARGUMENT_INSERT;
  }
  int USAGE;
}
