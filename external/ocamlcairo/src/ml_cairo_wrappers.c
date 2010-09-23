/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#define CAML_NAME_SPACE
#include <caml/alloc.h>

#include "ml_cairo_wrappers.h"

int
ml_pointer_compare (value a, value b)
{
  void *p1 = wPointer_val (void, a);
  void *p2 = wPointer_val (void, b);
  if (p1 == p2)
    return 0;
  else if (p1 < p2)
    return -1;
  else
    return 1;
}

long
ml_pointer_hash (value a)
{
  void *p = wPointer_val (void, a);
  return (long) p;
}
