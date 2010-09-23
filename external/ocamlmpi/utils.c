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

/* $Id: utils.c,v 1.2 2003/03/31 14:22:57 xleroy Exp $ */

/* Utility functions on arrays */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "camlmpi.h"

void caml_mpi_decode_intarray(value data, mlsize_t len)
{
  mlsize_t i;
  for (i = 0; i < len; i++) Field(data, i) = Long_val(Field(data, i));
}

void caml_mpi_encode_intarray(value data, mlsize_t len)
{
  mlsize_t i;
  for (i = 0; i < len; i++) Field(data, i) = Val_long(Field(data, i));
}

#ifdef ARCH_ALIGN_DOUBLE

double * caml_mpi_input_floatarray(value data, mlsize_t len)
{
  double * d = stat_alloc(len * sizeof(double));
  bcopy((double *) data, d, len * sizeof(double));
  return d;
}

double * caml_mpi_output_floatarray(value data, mlsize_t len)
{
  return stat_alloc(len * sizeof(double));
}

void caml_mpi_free_floatarray(double * d)
{
  if (d != NULL) stat_free(d);
}

void caml_mpi_commit_floatarray(double * d, value data, mlsize_t len)
{
  if (d != NULL) {
    bcopy(d, (double *) data, len * sizeof(double));
    stat_free(d);
  }
}

double * caml_mpi_input_floatarray_at_node(value data, mlsize_t len,
                                           value root, value comm)
{
  int myrank;
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root))
    return caml_mpi_input_floatarray(data, len);
  else
    return NULL;
}

double * caml_mpi_output_floatarray_at_node(value data, mlsize_t len,
                                           value root, value comm)
{
  int myrank;
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root))
    return caml_mpi_output_floatarray(data, len);
  else
    return NULL;
}

#endif
