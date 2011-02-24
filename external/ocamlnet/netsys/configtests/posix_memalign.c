#define _GNU_SOURCE

#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int e;
    void *addr;

    e = posix_memalign(&addr, 4096, 4096);
    
    return Val_int(e == 0 ? 0 : 1);
}
