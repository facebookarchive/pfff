#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <stdio.h>

#include "caml/mlvalues.h"
#include "caml/alloc.h"


static void *thread_main(void *arg) {
    pthread_exit(NULL);
}


/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    pthread_t thr;
    int code;

    code = pthread_create(&thr, NULL, thread_main, NULL);
    if (code != 0) return Val_int(1);

    code = pthread_join(thr, NULL);
    if (code != 0) return Val_int(1);

    return Val_int(0);
}
