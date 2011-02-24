/* $Id: netsys_c_htab.c 1497 2010-11-28 22:13:46Z gerd $ */

#include "netsys_c_htab.h"
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/* Define an FNV-1 hash function
   (see http://en.wikipedia.org/wiki/Fowler_Noll_Vo_hash)

   All artithmetic is unsigned!
*/
#ifdef ARCH_SIXTYFOUR
/* 64 bit */
#define P 1099511628211
#define S 0xcbf29ce484222325
#define H(x,n) \
  ((((((((((((((((S * P) ^ ((x >> 56) & 0xff)) * \
  P) ^ ((x >> 48) & 0xff)) * \
  P) ^ ((x >> 40) & 0xff)) * \
  P) ^ ((x >> 32) & 0xff)) * \
  P) ^ ((x >> 24) & 0xff)) * \
  P) ^ ((x >> 16) & 0xff)) * \
  P) ^ ((x >> 8) & 0xff)) * \
  P) ^ (x & 0xff)) % n
#else
/* 32 bit */
#define S 0x811c9dc5
#define P 16777619
#define H(x,n) \
  ((((((((S * P) ^ ((x >> 24) & 0xff)) * P) ^ ((x >> 16) & 0xff)) * \
  P) ^ ((x >> 8) & 0xff)) * P) ^ (x & 0xff)) % n
#endif


static void netsys_htab_add_1(struct htab *t, void *a1, void *a2)
{
    unsigned long i1, h1, size;
    struct htab_cell *table;

    table = t->table;
    size = t->table_size;
    i1 = (unsigned long) a1;
    h1 = H(i1, size);
    while (table[h1].orig_addr != NULL) {
        h1++;
        if (h1 == size) h1 = 0;
    }
    table[h1].orig_addr = a1;
    table[h1].relo_addr = a2;
    t->table_used++;
}


static int netsys_htab_grow(struct htab *t, unsigned long n)
{
    struct htab_cell *old_table;
    struct htab_cell *new_table;
    unsigned long k, old_size;

    if (n < t->table_size) return (-2);

    old_table = t->table;
    old_size = t->table_size;

    new_table = (struct htab_cell *) malloc(n * sizeof(struct htab_cell));
    if (new_table == NULL) {
        errno = ENOMEM;
        return (-1);
    } 

    for (k=0; k<n; k++) {
        new_table[k].orig_addr = NULL;
        new_table[k].relo_addr = NULL;
    }

    t->table = new_table;
    t->table_size = n;
    t->table_used = 0;

    if (old_table != NULL) {
        for (k=0; k<old_size; k++) {
            if (old_table[k].orig_addr != NULL) {
                netsys_htab_add_1(t, 
                                  old_table[k].orig_addr, 
                                  old_table[k].relo_addr);
            }
        }

        free(old_table);
    };

    return 0;
}


int netsys_htab_init(struct htab *t, unsigned long n)
{
    t->table = NULL;
    t->table_size = 0;
    t->table_used = 0;
    return netsys_htab_grow(t, n);
}


void netsys_htab_clear(struct htab *t) 
{
    unsigned long k, size;
    struct htab_cell *table;

    size = t->table_size;
    table = t->table;
    memset(table, 0, size * sizeof(struct htab_cell));
    t->table_used = 0;
}


int netsys_htab_add(struct htab *t, void *a1, void *a2)
{
    int code;

    if (a1 == NULL || a2 == NULL) return (-2);

    if (2 * t->table_used > t->table_size) {
        code = netsys_htab_grow(t, 2 * t->table_size);
        if (code < 0) return code;
    }

    netsys_htab_add_1(t, a1, a2);
    return 0;
}


int netsys_htab_lookup(struct htab *t, 
                       void *a1, void **a2p)
{
    unsigned long i1, h1, size;
    struct htab_cell *table;

    table = t->table;
    size = t->table_size;
    i1 = (unsigned long) a1;
    h1 = H(i1, size);
    while (table[h1].orig_addr != NULL && table[h1].orig_addr != a1) {
        h1++;
        if (h1 == size) h1 = 0;
    }
    if (table[h1].orig_addr == NULL)
        *a2p = NULL;
    else
        *a2p = table[h1].relo_addr;

    return 0;
}


void netsys_htab_free(struct htab *t)
{
    free(t->table);
    t->table = NULL;
}

