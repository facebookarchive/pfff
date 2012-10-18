package javax.print.event;
class PrintServiceAttributeListener {
}
class PrintServiceAttributeEvent {
  int attributes;
}
class PrintJobListener {
}
class PrintJobEvent {
  int reason;
  int REQUIRES_ATTENTION;
  int NO_MORE_EVENTS;
  int JOB_FAILED;
  int JOB_COMPLETE;
  int JOB_CANCELED;
  int DATA_TRANSFER_COMPLETE;
  int serialVersionUID;
}
class PrintJobAttributeListener {
}
class PrintJobAttributeEvent {
  int attributes;
  int serialVersionUID;
}
class PrintJobAdapter {
}
class PrintEvent {
}
