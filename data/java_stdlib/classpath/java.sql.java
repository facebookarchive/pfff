package java.sql;
class Types {
  int BOOLEAN;
  int DATALINK;
  int REF;
  int CLOB;
  int BLOB;
  int ARRAY;
  int STRUCT;
  int DISTINCT;
  int JAVA_OBJECT;
  int OTHER;
  int NULL;
  int LONGVARBINARY;
  int VARBINARY;
  int BINARY;
  int TIMESTAMP;
  int TIME;
  int DATE;
  int LONGVARCHAR;
  int VARCHAR;
  int CHAR;
  int DECIMAL;
  int NUMERIC;
  int DOUBLE;
  int REAL;
  int FLOAT;
  int BIGINT;
  int INTEGER;
  int SMALLINT;
  int TINYINT;
  int BIT;
}
class Timestamp {
  int nanos;
  int sbuf;
  int decimalFormat;
  int dateFormat;
  int serialVersionUID;
}
class Time {
  int sdf;
  int serialVersionUID;
}
class Struct {
}
class Statement {
  int NO_GENERATED_KEYS;
  int RETURN_GENERATED_KEYS;
  int EXECUTE_FAILED;
  int SUCCESS_NO_INFO;
  int CLOSE_ALL_RESULTS;
  int KEEP_CURRENT_RESULT;
  int CLOSE_CURRENT_RESULT;
}
class Savepoint {
}
class SQLWarning {
  int serialVersionUID;
}
class SQLPermission {
}
class SQLOutput {
}
class SQLInput {
}
class SQLException {
  int vendorCode;
  int SQLState;
  int next;
  int serialVersionUID;
}
class SQLData {
}
class ResultSetMetaData {
  int columnNullableUnknown;
  int columnNullable;
  int columnNoNulls;
}
class ResultSet {
  int CLOSE_CURSORS_AT_COMMIT;
  int HOLD_CURSORS_OVER_COMMIT;
  int CONCUR_UPDATABLE;
  int CONCUR_READ_ONLY;
  int TYPE_SCROLL_SENSITIVE;
  int TYPE_SCROLL_INSENSITIVE;
  int TYPE_FORWARD_ONLY;
  int FETCH_UNKNOWN;
  int FETCH_REVERSE;
  int FETCH_FORWARD;
}
class Ref {
}
class PreparedStatement {
}
class ParameterMetaData {
  int parameterModeOut;
  int parameterModeInOut;
  int parameterModeIn;
  int parameterModeUnknown;
  int parameterNullableUnknown;
  int parameterNullable;
  int parameterNoNulls;
}
class DriverPropertyInfo {
  int choices;
  int value;
  int required;
  int description;
  int name;
}
class DriverManager {
  int drivers;
  int login_timeout;
  int log_writer;
  int log_stream;
}
class Driver {
}
class Date {
  int sdf;
  int serialVersionUID;
}
class DatabaseMetaData {
  int sqlStateSQL99;
  int sqlStateXOpen;
  int attributeNullableUnknown;
  int attributeNullable;
  int attributeNoNulls;
  int tableIndexOther;
  int tableIndexHashed;
  int tableIndexClustered;
  int tableIndexStatistic;
  int typeSearchable;
  int typePredBasic;
  int typePredChar;
  int typePredNone;
  int typeNullableUnknown;
  int typeNullable;
  int typeNoNulls;
  int importedKeyNotDeferrable;
  int importedKeyInitiallyImmediate;
  int importedKeyInitiallyDeferred;
  int importedKeySetDefault;
  int importedKeyNoAction;
  int importedKeySetNull;
  int importedKeyRestrict;
  int importedKeyCascade;
  int versionColumnPseudo;
  int versionColumnNotPseudo;
  int versionColumnUnknown;
  int bestRowPseudo;
  int bestRowNotPseudo;
  int bestRowUnknown;
  int bestRowSession;
  int bestRowTransaction;
  int bestRowTemporary;
  int columnNullableUnknown;
  int columnNullable;
  int columnNoNulls;
  int procedureNullableUnknown;
  int procedureNullable;
  int procedureNoNulls;
  int procedureColumnResult;
  int procedureColumnReturn;
  int procedureColumnOut;
  int procedureColumnInOut;
  int procedureColumnIn;
  int procedureColumnUnknown;
  int procedureReturnsResult;
  int procedureNoResult;
  int procedureResultUnknown;
}
class DataTruncation {
  int transferSize;
  int read;
  int parameter;
  int index;
  int dataSize;
  int serialVersionUID;
}
class Connection {
  int TRANSACTION_SERIALIZABLE;
  int TRANSACTION_REPEATABLE_READ;
  int TRANSACTION_READ_COMMITTED;
  int TRANSACTION_READ_UNCOMMITTED;
  int TRANSACTION_NONE;
}
class Clob {
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
