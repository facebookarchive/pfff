package javax.management;
class ValueExp {
}
class StringValueExp {
  int val;
  int serialVersionUID;
}
class ServiceNotFoundException {
  int serialVersionUID;
}
class RuntimeOperationsException {
  int runtimeException;
  int serialVersionUID;
}
class RuntimeMBeanException {
  int runtimeException;
  int serialVersionUID;
}
class RuntimeErrorException {
  int error;
  int serialVersionUID;
}
class ReflectionException {
  int exception;
  int serialVersionUID;
}
class QueryExp {
}
class QueryEval {
  int server;
  int serialVersionUID;
}
class Query {
  class NumericValueExp {
    int val;
    int serialVersionUID;
  }
  class BooleanValueExp {
    int val;
    int serialVersionUID;
  }
  class OrQueryExp {
    int exp2;
    int exp1;
    int serialVersionUID;
  }
  class NotQueryExp {
    int exp;
    int serialVersionUID;
  }
  class InstanceOfQueryExp {
    int classNameValue;
    int serialVersionUID;
  }
  class InQueryExp {
    int valueList;
    int val;
    int serialVersionUID;
  }
  class BinaryRelQueryExp {
    int exp2;
    int exp1;
    int relOp;
    int serialVersionUID;
  }
  class BinaryOpValueExp {
    int exp2;
    int exp1;
    int op;
    int serialVersionUID;
  }
  class ClassAttributeValueExp {
    int serialVersionUID;
  }
  class BetweenQueryExp {
    int exp3;
    int exp2;
    int exp1;
    int serialVersionUID;
  }
  class QualifiedAttributeValueExp {
    int className;
    int serialVersionUID;
  }
  class MatchQueryExp {
    int pattern;
    int exp;
    int serialVersionUID;
  }
  class AndQueryExp {
    int exp2;
    int exp1;
    int serialVersionUID;
  }
  int EQ;
  int LE;
  int GE;
  int LT;
  int GT;
  int DIV;
  int TIMES;
  int MINUS;
  int PLUS;
}
class PersistentMBean {
}
class OperationsException {
  int serialVersionUID;
}
class ObjectName {
  int server;
  int propertyValuePattern;
  int propertyListPattern;
  int propertyListString;
  int properties;
  int domain;
  int WILDCARD;
  int serialVersionUID;
}
class ObjectInstance {
  int className;
  int name;
  int serialVersionUID;
}
class NotificationListener {
}
class NotificationFilterSupport {
  int enabledTypes;
  int serialVersionUID;
}
class NotificationFilter {
}
class NotificationEmitter {
}
class NotificationBroadcasterSupport {
  class DispatchTask {
    int notif;
    int ldata;
  }
  int listeners;
  int info;
  int executor;
}
class NotificationBroadcaster {
}
class Notification {
  int userData;
  int type;
  int timeStamp;
  int source;
  int sequenceNumber;
  int message;
  int serialVersionUID;
}
class NotCompliantMBeanException {
  int serialVersionUID;
}
class MalformedObjectNameException {
  int serialVersionUID;
}
class MBeanTrustPermission {
  int serialVersionUID;
}
class MBeanServerPermission {
  class MBeanServerPermissionCollection {
    class MBeanServerPermissionEnumeration {
      int done;
      int p;
    }
    int collectionPermission;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class MBeanServerNotification {
  int objectName;
  int UNREGISTRATION_NOTIFICATION;
  int REGISTRATION_NOTIFICATION;
  int serialVersionUID;
}
class MBeanServerInvocationHandler {
  int iface;
  int mxBean;
  int name;
  int conn;
}
class MBeanServerFactory {
  int servers;
  int builder;
}
class MBeanServerDelegateMBean {
}
class MBeanServerDelegate {
  int seqNo;
  int listeners;
  int id;
}
class MBeanServerConnection {
}
class MBeanServerBuilder {
}
class MBeanServer {
}
class MBeanRegistrationException {
  int serialVersionUID;
}
class MBeanRegistration {
}
class MBeanPermission {
  class NameHolder {
    int objectName;
    int member;
    int className;
  }
  int validSet;
  int actionSet;
  int actions;
  int serialVersionUID;
}
class MBeanParameterInfo {
  int type;
  int serialVersionUID;
}
class MBeanOperationInfo {
  int impact;
  int signature;
  int type;
  int UNKNOWN;
  int ACTION_INFO;
  int ACTION;
  int INFO;
  int serialVersionUID;
}
class MBeanNotificationInfo {
  int types;
  int serialVersionUID;
}
class MBeanInfo {
  int string;
  int notifications;
  int constructors;
  int operations;
  int attributes;
  int className;
  int description;
  int serialVersionUID;
}
class MBeanFeatureInfo {
  int string;
  int name;
  int description;
  int serialVersionUID;
}
class MBeanException {
  int exception;
  int serialVersionUID;
}
class MBeanConstructorInfo {
  int signature;
  int serialVersionUID;
}
class MBeanAttributeInfo {
  int is;
  int isRead;
  int isWrite;
  int attributeType;
  int serialVersionUID;
}
class ListenerNotFoundException {
  int serialVersionUID;
}
class JMX {
  int ORIGINAL_TYPE_FIELD;
  int OPEN_TYPE_FIELD;
  int MXBEAN_FIELD;
  int MIN_VALUE_FIELD;
  int MAX_VALUE_FIELD;
  int LEGAL_VALUES_FIELD;
  int INTERFACE_CLASS_NAME_FIELD;
  int IMMUTABLE_INFO_FIELD;
  int DEFAULT_VALUE_FIELD;
}
class JMRuntimeException {
}
class JMException {
}
class InvalidAttributeValueException {
}
class InvalidApplicationException {
  int val;
  int serialVersionUID;
}
class IntrospectionException {
  int serialVersionUID;
}
class InstanceNotFoundException {
  int serialVersionUID;
}
class InstanceAlreadyExistsException {
  int serialVersionUID;
}
class DynamicMBean {
}
class DescriptorRead {
}
class DescriptorAccess {
}
class Descriptor {
}
class DefaultLoaderRepository {
}
class BadStringOperationException {
  int op;
  int serialVersionUID;
}
class BadBinaryOpValueExpException {
  int exp;
  int serialVersionUID;
}
class BadAttributeValueExpException {
  int val;
  int serialVersionUID;
}
class AttributeValueExp {
  int attr;
  int serialVersionUID;
}
class AttributeNotFoundException {
}
class AttributeList {
  int serialVersionUID;
}
class AttributeChangeNotificationFilter {
  int enabledAttributes;
  int serialVersionUID;
}
class AttributeChangeNotification {
  int newValue;
  int oldValue;
  int attributeType;
  int attributeName;
  int ATTRIBUTE_CHANGE;
  int serialVersionUID;
}
class Attribute {
  int m_value;
  int m_name;
  int serialVersionUID;
}
