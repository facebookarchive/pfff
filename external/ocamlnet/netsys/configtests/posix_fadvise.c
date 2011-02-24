#define _GNU_SOURCE

#include <fcntl.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <errno.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int j, k;

    j = open("posix_fadvise.tmp", O_RDWR | O_CREAT, 0666);
    if (j == -1) return Val_int(1);   /* strange */

    k = posix_fadvise(j, 0, 0, POSIX_FADV_NORMAL);
    if (j == -1) return Val_int(1);

    return Val_int(0);
}
