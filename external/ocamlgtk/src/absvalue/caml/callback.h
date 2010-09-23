/**************************************************************************/
/*                Lablgtk                                                 */
/*                                                                        */
/*    This program is free software; you can redistribute it              */
/*    and/or modify it under the terms of the GNU Library General         */
/*    Public License as published by the Free Software Foundation         */
/*    version 2, with the exception described in file COPYING which       */
/*    comes with the library.                                             */
/*                                                                        */
/*    This program is distributed in the hope that it will be useful,     */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU Library General Public License for more details.                */
/*                                                                        */
/*    You should have received a copy of the GNU Library General          */
/*    Public License along with this program; if not, write to the        */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         */
/*    Boston, MA 02111-1307  USA                                          */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: callback.h 1347 2007-06-20 07:40:34Z guesdon $ */

/* Callbacks from C to Caml */

#ifndef CAML_GTK_CALLBACK_H
#define CAML_CALLBACK_H
#define CAML_GTK_CALLBACK_H

#ifndef CAML_NAME_SPACE
#include <caml/compatibility.h>
#endif
#include "mlvalues.h"

CAMLextern value caml_callback (value closure, value arg);
CAMLextern value caml_callback2 (value closure, value arg1, value arg2);
CAMLextern value caml_callback3 (value closure, value arg1, value arg2,
                                 value arg3);
CAMLextern value caml_callbackN (value closure, int narg, value args[]);

CAMLextern value caml_callback_exn (value closure, value arg);
CAMLextern value caml_callback2_exn (value closure, value arg1, value arg2);
CAMLextern value caml_callback3_exn (value closure,
                                     value arg1, value arg2, value arg3);
CAMLextern value caml_callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((intnat)(v) | 2)
#define Is_exception_result(v) (((intnat)(v) & 3) == 2)
#define Extract_exception(v) ((value)((intnat)(v) & ~3))

CAMLextern value * caml_named_value (char * name);

CAMLextern void caml_main (char ** argv);
CAMLextern void caml_startup (char ** argv);

CAMLextern int caml_callback_depth;

#endif
