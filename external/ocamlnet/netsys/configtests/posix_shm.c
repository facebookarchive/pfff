#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int ret;

    ret = shm_open("/thisFileDoesNotExist", O_RDONLY, 0);
    if (ret == -1 && errno == ENOENT) {
	return Val_int(0);  /* success */
    };
    return Val_int(1);
}
