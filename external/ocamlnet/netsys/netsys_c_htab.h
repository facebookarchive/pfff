/* $Id: netsys_c_htab.h 1497 2010-11-28 22:13:46Z gerd $ */

/* Hash table for netsys_c_mem.c */

#ifndef NETSYS_C_HTAB
#define NETSYS_C_HTAB

struct htab_cell {
    void *orig_addr;
    void *relo_addr;
    /* Both are NULL if the cell is unused. */
};

struct htab {
    struct htab_cell *table;
    unsigned long     table_size;
    unsigned long     table_used;
};

extern int netsys_htab_init(struct htab *t, unsigned long n);
/* Initializes the htab t for n cells

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error. On success, the structure [t] is initialized.
 */

extern void netsys_htab_clear(struct htab *t);
/* Re-initializes the htab t
 */

extern int netsys_htab_add(struct htab *t, 
                           void *orig_addr, void *relo_addr);
/* Adds the mapping from orig_addr to relo_addr to t.

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error.
 */

extern int netsys_htab_lookup(struct htab *t, 
                              void *orig_addr, void **relo_addr);
/* Looks up orig_addr in t, and returns the corresponding relo_addr
   (or NULL if not found).

   Return 0 on success, or (-1) on system error (errno), or (-2) on
   library error.
 */

extern void netsys_htab_free(struct htab *t);
/* Frees the memory allocated by t */

#endif
