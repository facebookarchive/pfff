/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include <assert.h>
#include <string.h>

#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>

#include "ml_cairo_wrappers.h"

#include <cairo.h>

/* cairo */
#define cairo_t_val(v)		wPointer_val(cairo_t, v)
value Val_cairo_t (cairo_t *);

#define cairo_surface_t_val(v)	wPointer_val(cairo_surface_t, v)
value Val_cairo_surface_t (cairo_surface_t *);
#define Val_cairo_surface_ref(p)	Val_cairo_surface_t (cairo_surface_reference(p))

#define cairo_pattern_t_val(v)	wPointer_val(cairo_pattern_t, v)
value Val_cairo_pattern_t (cairo_pattern_t *);
#define Val_cairo_pattern_ref(p)	Val_cairo_pattern_t (cairo_pattern_reference(p))

#define cairo_format_t_val(v) ((cairo_format_t) Int_val(v))
#define Val_cairo_format_t(v) Val_int(v)

#define cairo_antialias_t_val(v) ((cairo_antialias_t) Int_val(v))
#define Val_cairo_antialias_t(v) Val_int(v)

/* cairo_font */
#define cairo_font_face_t_val(v)	wPointer_val(cairo_font_face_t, v)
value Val_cairo_font_face_t (cairo_font_face_t *);
#define Val_cairo_font_face_ref(p)	Val_cairo_font_face_t (cairo_font_face_reference(p))

#define cairo_scaled_font_t_val(v)	wPointer_val(cairo_scaled_font_t, v)
value Val_cairo_scaled_font_t (cairo_scaled_font_t *);

#define cairo_font_options_t_val(v)	wPointer_val(cairo_font_options_t, v)
value Val_cairo_font_options_t (cairo_font_options_t *);

/* cairo_surface */
cairo_content_t cairo_content_t_val (value);


/* cairo_matrix */
#ifdef ARCH_ALIGN_DOUBLE
void	ml_convert_cairo_matrix_in	(value, cairo_matrix_t *);
value	ml_convert_cairo_matrix_out	(const cairo_matrix_t *);
#else
# define cairo_matrix_t_val(v)	(cairo_matrix_t *)(v)
# define cairo_matrix_alloc()	caml_alloc_small (6 * Double_wosize, Double_array_tag)
# define cairo_copy_matrix(dst, src)	memcpy (Bp_val(dst), Bp_val(src), 6 * Double_wosize * sizeof (value))
#endif

value	ml_cairo_point	(double, double);

cairo_glyph_t *	ml_convert_cairo_glypth_in	 (value v, int *);
value	Val_cairo_font_extents	(cairo_font_extents_t *);
value	Val_cairo_text_extents	(cairo_text_extents_t *);

/* cairo_status */
void ml_cairo_treat_status (cairo_status_t) Noreturn;
#define cairo_treat_status(s)   if (s != CAIRO_STATUS_SUCCESS) ml_cairo_treat_status (s)
#define check_cairo_status(cr)	cairo_treat_status (cairo_status (cairo_t_val (cr)))
#define check_surface_status(cr)	cairo_treat_status (cairo_surface_status (cairo_surface_t_val (cr)))
#define check_pattern_status(cr)	cairo_treat_status (cairo_pattern_status (cairo_pattern_t_val (cr)))
#define check_font_face_status(cr)	cairo_treat_status (cairo_font_face_status (cairo_font_face_t_val (cr)))
#define check_scaled_font_status(cr)	cairo_treat_status (cairo_scaled_font_status (cairo_scaled_font_t_val (cr)))
#define check_font_options_status(cr)	cairo_treat_status (cairo_font_options_status (cairo_font_options_t_val (cr)))
#define report_null_pointer()	ml_cairo_treat_status (CAIRO_STATUS_NULL_POINTER)

/* stream callbacks */
value	       *ml_cairo_make_closure	(value);
value	       *ml_cairo_make_root	(value);
cairo_status_t	ml_cairo_write_func	(void *, const unsigned char *, unsigned int);
cairo_status_t	ml_cairo_read_func	(void *, unsigned char *, unsigned int);
cairo_status_t	ml_cairo_unsafe_write_func	(void *, const unsigned char *, unsigned int);
cairo_status_t	ml_cairo_unsafe_read_func	(void *, unsigned char *, unsigned int);

void		ml_cairo_surface_set_stream_data	(cairo_surface_t *, value *);
void		ml_cairo_surface_set_image_data		(cairo_surface_t *, value);
