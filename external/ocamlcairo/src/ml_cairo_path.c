/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#define CAML_MOVE_TO_TAG  -1795134893L
#define CAML_LINE_TO_TAG   1059315789L
#define CAML_CLOSE_TAG    -1110065659L
#define CAML_CURVE_TO_TAG -2065449769L

static value
ml_cairo_fold_path (cairo_path_t *path, value f, value acc)
{
  CAMLparam2(f, acc);
  CAMLlocal5(var, t, p1, p2, p3);
  int i;

  cairo_treat_status (path->status);

  for (i = 0; i < path->num_data; i += path->data[i].header.length)
    {
      cairo_path_data_t *data = &path->data[i];
      switch (data->header.type)
	{
	case CAIRO_PATH_MOVE_TO:
	  {
	    p1 = ml_cairo_point (data[1].point.x, data[1].point.y);
	    var = caml_alloc_small (2, 0);
	    Field (var, 0) = CAML_MOVE_TO_TAG;
	    Field (var, 1) = p1;
	    break;
	  }

	case CAIRO_PATH_LINE_TO:
	  {
	    p1 = ml_cairo_point (data[1].point.x, data[1].point.y);
	    var = caml_alloc_small (2, 0);
	    Field (var, 0) = CAML_LINE_TO_TAG;
	    Field (var, 1) = p1;
	    break;
	  }

	case CAIRO_PATH_CURVE_TO:
	  {
	    p1 = ml_cairo_point (data[1].point.x, data[1].point.y);
	    p2 = ml_cairo_point (data[2].point.x, data[2].point.y);
	    p3 = ml_cairo_point (data[3].point.x, data[3].point.y);
	    t = caml_alloc_small (3, 0);
	    Field (t, 0) = p1;
	    Field (t, 1) = p2;
	    Field (t, 2) = p3;
	    var = caml_alloc_small (2, 0);
	    Field (var, 0) = CAML_CURVE_TO_TAG;
	    Field (var, 1) = t;
	    break;
	  }

	case CAIRO_PATH_CLOSE_PATH:
	  var = CAML_CLOSE_TAG;
	  break;
	}

      acc = caml_callback2_exn (f, acc, var);
      if (Is_exception_result (acc))
	break;
    }

  cairo_path_destroy (path);

  if (Is_exception_result (acc))
    caml_raise (Extract_exception (acc));

  CAMLreturn (acc);
}

CAMLprim value
ml_cairo_copy_path (value cr, value f, value init)
{
  return ml_cairo_fold_path (cairo_copy_path (cairo_t_val (cr)), f, init);
}

CAMLprim value
ml_cairo_copy_path_flat (value cr, value f, value init)
{
  return ml_cairo_fold_path (cairo_copy_path_flat (cairo_t_val (cr)), f, init);
}

/* append_path */
