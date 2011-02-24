#define _GNU_SOURCE
#define _XOPEN_SOURCE 700

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int fd;
    DIR *dir;
    fd = open(".", O_RDONLY, 0);
    dir = fdopendir(fd);
    if (dir == NULL) return Val_int(1);
    return Val_int(0);
}
