package android.database.sqlite;
class SqliteWrapper {
  int SQLITE_EXCEPTION_DETAIL_MESSAGE;
  int TAG;
}
class SQLiteTransactionListener {
}
class SQLiteTableLockedException {
}
class SQLiteStatementInfo {
  int readOnly;
  int columnNames;
  int numParameters;
}
class SQLiteStatement {
}
class SQLiteSession {
  class Transaction {
    int mChildFailed;
    int mMarkedSuccessful;
    int mListener;
    int mMode;
    int mParent;
  }
  int TRANSACTION_MODE_EXCLUSIVE;
  int TRANSACTION_MODE_IMMEDIATE;
  int TRANSACTION_MODE_DEFERRED;
  int mTransactionStack;
  int mTransactionPool;
  int mConnectionUseCount;
  int mConnectionFlags;
  int mConnection;
  int mConnectionPool;
}
class SQLiteReadOnlyDatabaseException {
}
class SQLiteQueryBuilder {
  int mStrict;
  int mFactory;
  int mDistinct;
  int mWhereClause;
  int mTables;
  int mProjectionMap;
  int sLimitPattern;
  int TAG;
}
class SQLiteQuery {
  int mCancellationSignal;
  int TAG;
}
class SQLiteProgram {
  int mBindArgs;
  int mNumParameters;
  int mColumnNames;
  int mReadOnly;
  int mSql;
  int mDatabase;
  int EMPTY_STRING_ARRAY;
}
class SQLiteOutOfMemoryException {
}
class SQLiteOpenHelper {
  int mErrorHandler;
  int mEnableWriteAheadLogging;
  int mIsInitializing;
  int mDatabase;
  int mNewVersion;
  int mFactory;
  int mName;
  int mContext;
  int DEBUG_STRICT_READONLY;
  int TAG;
}
class SQLiteMisuseException {
}
class SQLiteGlobal {
  int sDefaultPageSize;
  int sLock;
  int TAG;
}
class SQLiteFullException {
}
class SQLiteException {
}
class SQLiteDoneException {
}
class SQLiteDiskIOException {
}
class SQLiteDirectCursorDriver {
  int mQuery;
  int mCancellationSignal;
  int mSql;
  int mEditTable;
  int mDatabase;
}
class SQLiteDebug {
  class DbStats {
    int cache;
    int lookaside;
    int dbSize;
    int pageSize;
    int dbName;
  }
  class PagerStats {
    int dbStats;
    int largestMemAlloc;
    int pageCacheOverflow;
    int memoryUsed;
  }
  int DEBUG_LOG_SLOW_QUERIES;
  int DEBUG_SQL_TIME;
  int DEBUG_SQL_STATEMENTS;
  int DEBUG_SQL_LOG;
}
class SQLiteDatatypeMismatchException {
}
class SQLiteDatabaseLockedException {
}
class SQLiteDatabaseCorruptException {
}
class SQLiteDatabaseConfiguration {
  int customFunctions;
  int foreignKeyConstraintsEnabled;
  int locale;
  int maxSqlCacheSize;
  int openFlags;
  int label;
  int path;
  int MEMORY_DB_PATH;
  int EMAIL_IN_DB_PATTERN;
}
class SQLiteDatabase {
  class CustomFunction {
  }
  class CursorFactory {
  }
  int MAX_SQL_CACHE_SIZE;
  int ENABLE_WRITE_AHEAD_LOGGING;
  int CREATE_IF_NECESSARY;
  int NO_LOCALIZED_COLLATORS;
  int OPEN_READ_MASK;
  int OPEN_READONLY;
  int OPEN_READWRITE;
  int SQLITE_MAX_LIKE_PATTERN_LENGTH;
  int CONFLICT_VALUES;
  int CONFLICT_NONE;
  int CONFLICT_REPLACE;
  int CONFLICT_IGNORE;
  int CONFLICT_FAIL;
  int CONFLICT_ABORT;
  int CONFLICT_ROLLBACK;
  int mHasAttachedDbsLocked;
  int mConnectionPoolLocked;
  int mConfigurationLocked;
  int mCloseGuardLocked;
  int mLock;
  int mErrorHandler;
  int mCursorFactory;
  int mThreadSession;
  int sActiveDatabases;
  int EVENT_DB_CORRUPT;
  int TAG;
}
class SQLiteCustomFunction {
  int callback;
  int numArgs;
  int name;
}
class SQLiteCursorTest {
  int TABLE_NAME;
  int mDatabaseFile;
  int mDatabase;
}
class SQLiteCursorDriver {
}
class SQLiteCursor {
  int mStackTrace;
  int mColumnNameMap;
  int mCursorWindowCapacity;
  int mCount;
  int mDriver;
  int mQuery;
  int mColumns;
  int mEditTable;
  int NO_COUNT;
  int TAG;
}
class SQLiteConstraintException {
}
class SQLiteConnectionPool {
  class ConnectionWaiter {
    int mNonce;
    int mException;
    int mAssignedConnection;
    int mConnectionFlags;
    int mSql;
    int mWantPrimaryConnection;
    int mPriority;
    int mStartTime;
    int mThread;
    int mNext;
  }
  int CONNECTION_FLAG_INTERACTIVE;
  int CONNECTION_FLAG_PRIMARY_CONNECTION_AFFINITY;
  int CONNECTION_FLAG_READ_ONLY;
  int mAcquiredConnections;
  class AcquiredConnectionStatus {
    int DISCARD;
    int RECONFIGURE;
    int NORMAL;
  }
  int mAvailablePrimaryConnection;
  int mAvailableNonPrimaryConnections;
  int mConnectionWaiterQueue;
  int mConnectionWaiterPool;
  int mNextConnectionId;
  int mIsOpen;
  int mMaxConnectionPoolSize;
  int mConfiguration;
  int mConnectionLeaked;
  int mLock;
  int mCloseGuard;
  int CONNECTION_POOL_BUSY_MILLIS;
  int TAG;
}
class SQLiteConnection {
  class Operation {
    int mCookie;
    int mException;
    int mFinished;
    int mBindArgs;
    int mSql;
    int mKind;
    int mEndTime;
    int mStartTime;
    int sDateFormat;
  }
  class OperationLog {
    int mGeneration;
    int mIndex;
    int mOperations;
    int COOKIE_INDEX_MASK;
    int COOKIE_GENERATION_SHIFT;
    int MAX_RECENT_OPERATIONS;
  }
  class PreparedStatementCache {
  }
  class PreparedStatement {
    int mInUse;
    int mInCache;
    int mReadOnly;
    int mType;
    int mNumParameters;
    int mStatementPtr;
    int mSql;
    int mPoolNext;
  }
  int mCancellationSignalAttachCount;
  int mOnlyAllowReadOnlyOperations;
  int mConnectionPtr;
  int mRecentOperations;
  int mPreparedStatementPool;
  int mPreparedStatementCache;
  int mIsReadOnlyConnection;
  int mIsPrimaryConnection;
  int mConnectionId;
  int mConfiguration;
  int mPool;
  int mCloseGuard;
  int TRIM_SQL_PATTERN;
  int EMPTY_BYTE_ARRAY;
  int EMPTY_STRING_ARRAY;
  int DEBUG;
  int TAG;
}
class SQLiteClosable {
  int mReferenceCount;
}
class SQLiteCantOpenDatabaseException {
}
class SQLiteBlobTooBigException {
}
class SQLiteBindOrColumnIndexOutOfRangeException {
}
class SQLiteAccessPermException {
}
class SQLiteAbortException {
}
class DatabaseObjectNotClosedException {
  int s;
}
