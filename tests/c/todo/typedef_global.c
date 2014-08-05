/*
 */

static Recover  *rtab;


#define csr32w(c, r, v) (*((c)->nic+((r)/4)) = (v))
 
static CtlrEtherIgbe* igbectlrhead;
