#define _GNU_SOURCE

#include <errno.h>
#include <unistd.h>
#include <semaphore.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    sem_t s;
    int code;

    code = sem_init(&s, 0, 0);
    sem_unlink("/foo_khfkshdfhf");

    return Val_int(code);
}
