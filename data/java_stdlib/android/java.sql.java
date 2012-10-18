package java.sql;
class Wrapper {
}
class Types {
  int SQLXML;
  int NCLOB;
  int LONGNVARCHAR;
  int NVARCHAR;
  int NCHAR;
  int ROWID;
  int VARCHAR;
  int VARBINARY;
  int TINYINT;
  int TIMESTAMP;
  int TIME;
  int STRUCT;
  int SMALLINT;
  int REF;
  int REAL;
  int OTHER;
  int NUMERIC;
  int NULL;
  int LONGVARCHAR;
  int LONGVARBINARY;
  int JAVA_OBJECT;
  int INTEGER;
  int FLOAT;
  int DOUBLE;
  int DISTINCT;
  int DECIMAL;
  int DATE;
  int DATALINK;
  int CLOB;
  int CHAR;
  int BOOLEAN;
  int BLOB;
  int BIT;
  int BINARY;
  int BIGINT;
  int ARRAY;
}
class Timestamp {
  int PADDING;
  int TIME_FORMAT_REGEX;
  int nanos;
  int serialVersionUID;
}
class Time {
  int PADDING;
  int serialVersionUID;
}
class Struct {
}
class Statement {
  int SUCCESS_NO_INFO;
  int RETURN_GENERATED_KEYS;
  int NO_GENERATED_KEYS;
  int KEEP_CURRENT_RESULT;
  int EXECUTE_FAILED;
  int CLOSE_CURRENT_RESULT;
  int CLOSE_ALL_RESULTS;
}
class Savepoint {
}
class SQLXML {
}
class SQLWarning {
  int serialVersionUID;
}
class SQLTransientException {
  int serialVersionUID;
}
class SQLTransientConnectionException {
  int serialVersionUID;
}
class SQLTransactionRollbackException {
  int serialVersionUID;
}
class SQLTimeoutException {
  int serialVersionUID;
}
class SQLSyntaxErrorException {
  int serialVersionUID;
}
class SQLRecoverableException {
  int serialVersionUID;
}
class SQLPermission {
}
class SQLOutput {
}
class SQLNonTransientException {
  int serialVersionUID;
}
class SQLNonTransientConnectionException {
  int serialVersionUID;
}
class SQLInvalidAuthorizationSpecException {
  int serialVersionUID;
}
class SQLIntegrityConstraintViolationException {
  int serialVersionUID;
}
class SQLInput {
}
class SQLFeatureNotSupportedException {
  int serialVersionUID;
}
class SQLException {
  class InternalIterator {
    int current;
  }
  int next;
  int vendorCode;
  int SQLState;
  int serialVersionUID;
}
class SQLDataException {
  int serialVersionUID;
}
class SQLData {
}
class SQLClientInfoException {
  int failedProperties;
  int serialVersionUID;
}
class RowIdLifetime {
  int ROWID_VALID_FOREVER;
  int ROWID_VALID_TRANSACTION;
  int ROWID_VALID_SESSION;
  int ROWID_VALID_OTHER;
  int ROWID_UNSUPPORTED;
}
class RowId {
}
class ResultSetMetaData {
  int columnNullableUnknown;
  int columnNullable;
  int columnNoNulls;
}
class ResultSet {
  int TYPE_SCROLL_SENSITIVE;
  int TYPE_SCROLL_INSENSITIVE;
  int TYPE_FORWARD_ONLY;
  int FETCH_UNKNOWN;
  int FETCH_REVERSE;
  int FETCH_FORWARD;
  int CONCUR_UPDATABLE;
  int CONCUR_READ_ONLY;
  int HOLD_CURSORS_OVER_COMMIT;
  int CLOSE_CURSORS_AT_COMMIT;
}
class Ref {
}
class PreparedStatement {
}
class ParameterMetaData {
  int parameterNullableUnknown;
  int parameterNullable;
  int parameterNoNulls;
  int parameterModeUnknown;
  int parameterModeOut;
  int parameterModeInOut;
  int parameterModeIn;
}
class NClob {
}
class DriverPropertyInfo {
  int value;
  int required;
  int name;
  int description;
  int choices;
}
class DriverManager {
  int logPermission;
  int theDrivers;
  int loginTimeout;
  int thePrintWriter;
  int thePrintStream;
}
class Driver {
}
class Date {
  int PADDING;
  int serialVersionUID;
}
class DatabaseMetaData {
  int functionReturnsTable;
  int functionNoTable;
  int functionResultUnknown;
  int functionNullableUnknown;
  int functionNullable;
  int functionNoNulls;
  int functionColumnResult;
  int functionColumnUnknown;
  int functionReturn;
  int functionColumnOut;
  int functionColumnInOut;
  int functionColumnIn;
  int sqlStateSQL;
  int versionColumnUnknown;
  int versionColumnPseudo;
  int versionColumnNotPseudo;
  int typeSearchable;
  int typePredNone;
  int typePredChar;
  int typePredBasic;
  int typeNullableUnknown;
  int typeNullable;
  int typeNoNulls;
  int tableIndexStatistic;
  int tableIndexOther;
  int tableIndexHashed;
  int tableIndexClustered;
  int sqlStateXOpen;
  int sqlStateSQL99;
  int procedureReturnsResult;
  int procedureResultUnknown;
  int procedureNullableUnknown;
  int procedureNullable;
  int procedureNoResult;
  int procedureNoNulls;
  int procedureColumnUnknown;
  int procedureColumnReturn;
  int procedureColumnResult;
  int procedureColumnOut;
  int procedureColumnInOut;
  int procedureColumnIn;
  int importedKeySetNull;
  int importedKeySetDefault;
  int importedKeyRestrict;
  int importedKeyNotDeferrable;
  int importedKeyNoAction;
  int importedKeyInitiallyImmediate;
  int importedKeyInitiallyDeferred;
  int importedKeyCascade;
  int columnNullableUnknown;
  int columnNullable;
  int columnNoNulls;
  int bestRowUnknown;
  int bestRowTransaction;
  int bestRowTemporary;
  int bestRowSession;
  int bestRowPseudo;
  int bestRowNotPseudo;
  int attributeNullableUnknown;
  int attributeNullable;
  int attributeNoNulls;
}
class DataTruncation {
  int THE_ERROR_CODE;
  int THE_SQLSTATE_WRITE;
  int THE_SQLSTATE_READ;
  int THE_REASON;
  int transferSize;
  int dataSize;
  int read;
  int parameter;
  int index;
  int serialVersionUID;
}
class Connection {
  int TRANSACTION_SERIALIZABLE;
  int TRANSACTION_REPEATABLE_READ;
  int TRANSACTION_READ_UNCOMMITTED;
  int TRANSACTION_READ_COMMITTED;
  int TRANSACTION_NONE;
}
class Clob {
}
class ClientInfoStatus {
  int REASON_VALUE_TRUNCATED;
  int REASON_VALUE_INVALID;
  int REASON_UNKNOWN_PROPERTY;
  int REASON_UNKNOWN;
}
class CallableStatement {
}
class Blob {
}
class BatchUpdateException {
  int updateCounts;
  int serialVersionUID;
}
class Array {
}
