/* $Id: netsys_c_gprof.c 1401 2010-02-03 23:25:36Z gerd $ */

#include "netsys_c.h"


#ifdef HAVE_GPROF
extern int moncontrol(int);
#endif

value netsys_moncontrol(value flag) {
#ifdef HAVE_GPROF
    moncontrol(Bool_val(flag));
#endif
    return Val_unit;
}
