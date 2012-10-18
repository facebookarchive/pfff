package javax.transaction;
class UserTransaction {
}
class TransactionRolledbackException {
}
class TransactionRequiredException {
}
class TransactionManager {
}
class Transaction {
}
class SystemException {
  int errorCode;
}
class Synchronization {
}
class Status {
  int STATUS_ROLLING_BACK;
  int STATUS_COMMITTING;
  int STATUS_PREPARING;
  int STATUS_NO_TRANSACTION;
  int STATUS_UNKNOWN;
  int STATUS_ROLLEDBACK;
  int STATUS_COMMITTED;
  int STATUS_PREPARED;
  int STATUS_MARKED_ROLLBACK;
  int STATUS_ACTIVE;
}
class RollbackException {
}
class NotSupportedException {
}
class InvalidTransactionException {
}
class HeuristicRollbackException {
}
class HeuristicMixedException {
}
class HeuristicCommitException {
}
