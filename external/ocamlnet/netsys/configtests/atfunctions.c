#define _GNU_SOURCE
#define _ATFILE_SOURCE

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

/* Solaris currently has openat and unlinkat but not the other
 * at functions. We want them as a whole only - so test here
 * explicitly for one of the other functions, faccessat
 */

value check(value dummy) {
    int fd1, fd2;

    fd1 = open(".", O_RDONLY, 0);
    if (fd1 == -1) return Val_int(1);
    fd2 = openat(fd1, "atfiletest", O_RDWR|O_CREAT, 0600);
    if (fd2 == -1) return Val_int(1);
    if (faccessat(fd1, "atfiletest", F_OK, 0) == -1) return Val_int(1);
    if (unlinkat(fd1, "atfiletest", 0) == -1) return Val_int(1);
    return Val_int(0);
}
