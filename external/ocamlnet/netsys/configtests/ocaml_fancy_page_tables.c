#include <caml/config.h>
#include <caml/mlvalues.h>

int caml_page_table_add(int kind, void * start, void * end);
#define Page_mask ((uintnat) -1 << Page_log)

/* reminder: we return the exit code, and 0 means success */

value check(value dummy) {
    int e;
    char b[100000];
    uintnat addr;

    addr = ((uintnat) b) & Page_mask;
    e = caml_page_table_add(4, (void *) addr, (void *) addr + (1 << Page_log));
    
    return Val_int(e == 0 ? 0 : 1);
}
