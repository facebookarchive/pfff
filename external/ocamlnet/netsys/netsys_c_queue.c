/* $Id: netsys_c_queue.c 1497 2010-11-28 22:13:46Z gerd $ */

#include "netsys_c_queue.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>


int netsys_queue_init(struct nqueue *q, unsigned long n)
{
    void *t;

    t = malloc(n * sizeof(void *));
    if (t == NULL) {
	errno = ENOMEM;
	return (-1);
    }

    q->table = (void **) t;
    q->table_size = n;
    q->table_start = 0;
    q->table_end = 0;

    return 0;
}


void netsys_queue_clear(struct nqueue *q)
{
    q->table_start = 0;
    q->table_end = 0;
}


long netsys_queue_size(struct nqueue *q)
{
    if (q->table_end >= q->table_start)
	return q->table_end - q->table_start;
    else
	return (q->table_size - q->table_end) + q->table_start;
}


int netsys_queue_add(struct nqueue *q, void *elem)
{
    int code;
    unsigned long new_end;

    new_end = q->table_end + 1;
    if (new_end == q->table_size) new_end = 0;

    if (new_end == q->table_start) {
	/* We have to resize */
	struct nqueue q1;
	int n;
	
	code = netsys_queue_init(&q1, q->table_size * 2);
	if (code != 0) return code;
	
	if (q->table_start <= q->table_end) {
	    n = q->table_end - q->table_start;
	    memcpy(q1.table, 
		   q->table + q->table_start,
		   n * sizeof(void *));
	}
	else {
	    int n1;
	    n1 = q->table_size - q->table_start;
	    memcpy(q1.table, 
		   q->table + q->table_start,
		   n1 * sizeof(void *));
	    memcpy(q1.table + n1,
		   q->table,
		   q->table_end * sizeof(void *));
	    n = n1 + q->table_end;
	}

	free(q->table);
	q->table = q1.table;
	q->table_size = q1.table_size;
	q->table_start = 0;
	q->table_end = n;
	new_end = n+1;
    }

    *(q->table + q->table_end) = elem;
    q->table_end = new_end;

    return 0;
}


int netsys_queue_take(struct nqueue *q, void **elem)
{
    unsigned long new_start;

    if (q->table_start == q->table_end) {
	*elem = NULL;
	return (-3);
    }

    new_start = q->table_start + 1;
    if (new_start == q->table_size) new_start = 0;

    *elem = *(q->table + q->table_start);
    q->table_start = new_start;

    return 0;
}


void netsys_queue_free(struct nqueue *q)
{
    free(q->table);
    q->table = NULL;
}
