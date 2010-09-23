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

/* $Id: comm.c,v 1.7 2003/03/31 14:38:36 xleroy Exp $ */

/* Handling of communicators */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "camlmpi.h"

static void caml_mpi_finalize_comm(value v)
{
  MPI_Comm_free(&Comm_val(v));
}

value caml_mpi_alloc_comm(MPI_Comm c)
{
  value res =
    alloc_final(1 + (sizeof(MPI_Comm) + sizeof(value) - 1) / sizeof(value),
                caml_mpi_finalize_comm, 1, 100);
  Comm_val(res) = c;
  return res;
}

value caml_mpi_get_comm_world(value unit)
{
  return caml_mpi_alloc_comm(MPI_COMM_WORLD);
}

value caml_mpi_comm_size(value comm)
{
  int size;
  MPI_Comm_size(Comm_val(comm), &size);
  return Val_int(size);
}

value caml_mpi_comm_rank(value comm)
{
  int rank;
  MPI_Comm_rank(Comm_val(comm), &rank);
  return Val_int(rank);
}

value caml_mpi_comm_compare(value comm1, value comm2)
{
  int res;
  MPI_Comm_compare(Comm_val(comm1), Comm_val(comm2), &res);
  return Val_bool(res);
}

value caml_mpi_comm_split(value comm, value color, value key)
{
  MPI_Comm newcomm;
  MPI_Comm_split(Comm_val(comm), Int_val(color), Int_val(key), &newcomm);
  return caml_mpi_alloc_comm(newcomm);
}

value caml_mpi_get_undefined(value unit)
{
  return Val_int(MPI_UNDEFINED);
}

value caml_mpi_cart_create(value comm, value vdims, value vperiods,
                           value reorder)
{
  int ndims = Wosize_val(vdims);
  int * dims = stat_alloc(ndims * sizeof(int));
  int * periods = stat_alloc(ndims * sizeof(int));
  int i;
  MPI_Comm newcomm;

  for (i = 0; i < ndims; i++) dims[i] = Int_val(Field(vdims, i));
  for (i = 0; i < ndims; i++) periods[i] = Int_val(Field(vperiods, i));
  MPI_Cart_create(Comm_val(comm), ndims, dims, periods, 
                  Bool_val(reorder), &newcomm);
  stat_free(dims);
  stat_free(periods);
  return caml_mpi_alloc_comm(newcomm);
}

value caml_mpi_dims_create(value vnnodes, value vdims)
{
  int ndims = Wosize_val(vdims);
  int * dims = stat_alloc(ndims * sizeof(int));
  int i;
  value res;

  for (i = 0; i < ndims; i++) dims[i] = Int_val(Field(vdims, i));
  MPI_Dims_create(Int_val(vnnodes), ndims, dims);
  res = alloc_tuple(ndims);
  for (i = 0; i < ndims; i++) Field(res, i) = Val_int(dims[i]);
  stat_free(dims);
  return res;
}

value caml_mpi_cart_rank(value comm, value vcoords)
{
  int ndims = Wosize_val(vcoords);
  int * coords = stat_alloc(ndims * sizeof(int));
  int i, rank;

  for (i = 0; i < ndims; i++) coords[i] = Int_val(Field(vcoords, i));
  MPI_Cart_rank(Comm_val(comm), coords, &rank);
  stat_free(coords);
  return Val_int(rank);
}

value caml_mpi_cart_coords(value comm, value rank)
{
  int ndims, i;
  int * coords;
  value res;

  MPI_Cartdim_get(Comm_val(comm), &ndims);
  coords = stat_alloc(ndims * sizeof(int));
  MPI_Cart_coords(Comm_val(comm), Int_val(rank), ndims, coords);
  res = alloc_tuple(ndims);
  for (i = 0; i < ndims; i++) Field(res, i) = Val_int(coords[i]);
  stat_free(coords);
  return res;
}

value caml_mpi_comm_create(value comm, value group)
{
  MPI_Comm newcomm;
  MPI_Comm_create(Comm_val(comm), Group_val(group), &newcomm);
  return caml_mpi_alloc_comm(newcomm);
}
