package javax.management.remote;
class TargetedNotification {
  int id;
  int notif;
  int serialVersionUID;
}
class NotificationResult {
  int targetedNotifications;
  int nextSequenceNumber;
  int earliestSequenceNumber;
  int serialVersionUID;
}
