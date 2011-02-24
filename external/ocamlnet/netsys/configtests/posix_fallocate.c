#define _GNU_SOURCE

#include <fcntl.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <errno.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int j, k;
    off_t offset, len;

    j = open("posix_fallocate.tmp", O_RDWR | O_CREAT, 0666);
    if (j == -1) return Val_int(1);   /* strange */

    offset = 0;
    len = 1;
    k = posix_fallocate(j, offset, len);
    if (k != 0 && errno==ENOSYS) return Val_int(1);
    /* Careful: Some OS supports fallocate but not all filesystems! */

    return Val_int(0);
}
