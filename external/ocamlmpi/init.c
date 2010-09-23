/***********************************************************************/
/*                                                                     */
/*                         The Caml/MPI interface                      */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id: init.c,v 1.5 2003/03/31 14:38:37 xleroy Exp $ */

/* Initialization and error handling */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlmpi.h"

/* Error handling */

static value * caml_mpi_exn = NULL;

static void caml_mpi_error_handler(MPI_Comm * comm, int * errcode, ...)
{
  char errmsg[MPI_MAX_ERROR_STRING + 1];
  int resultlen;
  value msg;

  MPI_Error_string(*errcode, errmsg, &resultlen);
  msg = copy_string(errmsg);
  if (caml_mpi_exn == NULL) {
    caml_mpi_exn = caml_named_value("Mpi.Error");
    if (caml_mpi_exn == NULL)
      invalid_argument("Exception MPI.Error not initialized");
  }
  raise_with_arg(*caml_mpi_exn, msg);
}

/* Initialization and finalization */

value caml_mpi_init(value arguments)
{
  int argc, i;
  char ** argv;
  MPI_Errhandler hdlr;

  argc = Wosize_val(arguments);
  argv = stat_alloc((argc + 1) * sizeof(char *));
  for (i = 0; i < argc; i++) argv[i] = String_val(Field(arguments, i));
  argv[i] = NULL;
  MPI_Init(&argc, &argv);
  /* Register an error handler */
  MPI_Errhandler_create((MPI_Handler_function *)caml_mpi_error_handler, &hdlr);
  MPI_Errhandler_set(MPI_COMM_WORLD, hdlr);
  return Val_unit;
}

value caml_mpi_finalize(value unit)
{
  MPI_Finalize();
  return Val_unit;
}

value caml_mpi_wtime(value unit)
{
  return copy_double(MPI_Wtime());
}

