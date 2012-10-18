package com.android.common.contacts;
class DataUsageStatUpdater {
  int mResolver;
  class DataUsageFeedback {
    int USAGE_TYPE_SHORT_TEXT;
    int USAGE_TYPE_LONG_TEXT;
    int USAGE_TYPE_CALL;
    int USAGE_TYPE;
    int FEEDBACK_URI;
  }
  int TAG;
}
class BaseEmailAddressAdapter {
  int mHandler;
  int mPreferredMaxResultCount;
  int mAccount;
  int mDirectoriesLoaded;
  int mContentResolver;
  class DirectoryPartitionFilter {
    int mLimit;
    int mDirectoryId;
    int mPartitionIndex;
  }
  class DefaultPartitionFilter {
  }
  int SEARCHING_CURSOR_MARKER;
  class DirectoryListQuery {
    int TYPE_RESOURCE_ID;
    int PACKAGE_NAME;
    int DISPLAY_NAME;
    int ACCOUNT_TYPE;
    int ACCOUNT_NAME;
    int ID;
    int PROJECTION;
    int DIRECTORY_TYPE_RESOURCE_ID;
    int DIRECTORY_PACKAGE_NAME;
    int DIRECTORY_DISPLAY_NAME;
    int DIRECTORY_ACCOUNT_TYPE;
    int DIRECTORY_ACCOUNT_NAME;
    int DIRECTORY_ID;
    int URI;
  }
  class EmailQuery {
    int ADDRESS;
    int NAME;
    int PROJECTION;
  }
  class DirectoryPartition {
    int filter;
    int constraint;
    int loading;
    int accountType;
    int accountName;
    int displayName;
    int directoryType;
    int directoryId;
  }
  int MESSAGE_SEARCH_PENDING;
  int MESSAGE_SEARCH_PENDING_DELAY;
  int ALLOWANCE_FOR_DUPLICATES;
  int DEFAULT_PREFERRED_MAX_RESULT_COUNT;
  int PRIMARY_ACCOUNT_TYPE;
  int PRIMARY_ACCOUNT_NAME;
  int LIMIT_PARAM_KEY;
  int DIRECTORY_PARAM_KEY;
  int DIRECTORY_LOCAL_INVISIBLE;
  int TAG;
}
