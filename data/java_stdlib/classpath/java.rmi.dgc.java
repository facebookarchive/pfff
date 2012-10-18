package java.rmi.dgc;
class VMID {
  int uid;
  int hash;
  int addr;
  int localAddr;
  int areWeUnique;
  int serialVersionUID;
}
class Lease {
  int value;
  int vmid;
  int serialVersionUID;
}
class DGC {
}
