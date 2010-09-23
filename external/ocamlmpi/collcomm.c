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

/* $Id: collcomm.c,v 1.5 2003/03/31 14:38:36 xleroy Exp $ */

/* Group communication */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "camlmpi.h"

/* Barrier synchronization */

value caml_mpi_barrier(value comm)
{
  MPI_Barrier(Comm_val(comm));
  return Val_unit;
}

/* Broadcast */

value caml_mpi_broadcast(value buffer, value root, value comm)
{
  MPI_Bcast(String_val(buffer), string_length(buffer), MPI_BYTE,
            Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_broadcast_int(value data, value root, value comm)
{
  long n = Long_val(data);
  MPI_Bcast(&n, 1, MPI_LONG, Int_val(root), Comm_val(comm));
  return Val_long(n);
}

value caml_mpi_broadcast_float(value data, value root, value comm)
{
  double d = Double_val(data);
  MPI_Bcast(&d, 1, MPI_DOUBLE, Int_val(root), Comm_val(comm));
  return copy_double(d);
}

value caml_mpi_broadcast_intarray(value data, value root, value comm)
{
  MPI_Bcast(&Field(data, 0), Wosize_val(data), MPI_LONG,
            Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_broadcast_floatarray(value data, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  MPI_Bcast(d, len, MPI_DOUBLE, Int_val(root), Comm_val(comm));
  caml_mpi_commit_floatarray(d, data, len);
  return Val_unit;
}

/* Scatter */

static void caml_mpi_counts_displs(value lengths,
                                   /* out */ int ** counts,
                                   /* out */ int ** displs)
{
  int size, disp, i;

  size = Wosize_val(lengths);
  if (size > 0) {
    *counts = stat_alloc(size * sizeof(int));
    *displs = stat_alloc(size * sizeof(int));
    for (i = 0, disp = 0; i < size; i++) {
      (*counts)[i] = Int_val(Field(lengths, i));
      (*displs)[i] = disp;
      disp += (*counts)[i];
    }
  } else {
    *counts = NULL;
    *displs = NULL;
  }
}

value caml_mpi_scatter(value sendbuf, value sendlengths, 
                       value recvbuf,
                       value root, value comm)
{
  int * sendcounts, * displs;

  caml_mpi_counts_displs(sendlengths, &sendcounts, &displs);
  MPI_Scatterv(String_val(sendbuf), sendcounts, displs, MPI_BYTE,
               String_val(recvbuf), string_length(recvbuf), MPI_BYTE,
               Int_val(root), Comm_val(comm));
  if (sendcounts != NULL) {
    stat_free(sendcounts);
    stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_scatter_int(value data, value root, value comm)
{
  value n;

  MPI_Scatter(&Field(data, 0), 1, MPI_LONG,
              &n, 1, MPI_LONG,
              Int_val(root), Comm_val(comm));
  return n;
}

value caml_mpi_scatter_float(value data, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * src = caml_mpi_input_floatarray(data, len);
  double dst;
  MPI_Scatter(src, 1, MPI_DOUBLE, &dst, 1, MPI_DOUBLE,
              Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(src);
  return copy_double(dst);
}

value caml_mpi_scatter_intarray(value source, value dest,
                                value root, value comm)
{
  mlsize_t len = Wosize_val(dest);
  MPI_Scatter(&Field(source, 0), len, MPI_LONG,
              &Field(dest, 0), len, MPI_LONG,
              Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_scatter_floatarray(value source, value dest,
                                  value root, value comm)
{
  mlsize_t srclen = Wosize_val(source) / Double_wosize;
  mlsize_t len = Wosize_val(dest) / Double_wosize;
  double * src = caml_mpi_input_floatarray_at_node(source, srclen, root, comm);
  double * dst = caml_mpi_output_floatarray(dest, len);

  MPI_Scatter(src, len, MPI_DOUBLE, dst, len, MPI_DOUBLE,
              Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(src);
  caml_mpi_commit_floatarray(dst, dest, len);
  return Val_unit;
}

/* Gather */

value caml_mpi_gather(value sendbuf,
                      value recvbuf, value recvlengths,
                      value root, value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Gatherv(String_val(sendbuf), string_length(sendbuf), MPI_BYTE,
              String_val(recvbuf), recvcounts, displs, MPI_BYTE,
              Int_val(root), Comm_val(comm));
  if (recvcounts != NULL) {
    stat_free(recvcounts);
    stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_gather_int(value data, value result, value root, value comm)
{
  MPI_Gather(&data, 1, MPI_LONG,
             &Field(result, 0), 1, MPI_LONG,
             Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_gather_intarray(value data, value result,
                               value root, value comm)
{
  mlsize_t len = Wosize_val(data);
  MPI_Gather(&Field(data, 0), len, MPI_LONG,
             &Field(result, 0), len, MPI_LONG,
             Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_gather_float(value data, value result, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  mlsize_t reslen = Wosize_val(result) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res =
    caml_mpi_output_floatarray_at_node(result, reslen, root, comm);
  MPI_Gather(d, len, MPI_DOUBLE, res, len, MPI_DOUBLE,
             Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, reslen);
  return Val_unit;
}

/* Gather to all */

value caml_mpi_allgather(value sendbuf,
                         value recvbuf, value recvlengths,
                         value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Allgatherv(String_val(sendbuf), string_length(sendbuf), MPI_BYTE,
                 String_val(recvbuf), recvcounts, displs, MPI_BYTE,
                 Comm_val(comm));
  stat_free(recvcounts);
  stat_free(displs);
  return Val_unit;
}

value caml_mpi_allgather_int(value data, value result, value comm)
{
  MPI_Allgather(&data, 1, MPI_LONG,
                &Field(result, 0), 1, MPI_LONG,
                Comm_val(comm));
  return Val_unit;
}

value caml_mpi_allgather_intarray(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data);
  MPI_Allgather(&Field(data, 0), len, MPI_LONG,
                &Field(result, 0), len, MPI_LONG,
                Comm_val(comm));
  return Val_unit;
}

value caml_mpi_allgather_float(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  mlsize_t reslen = Wosize_val(result) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, reslen);

  MPI_Allgather(d, len, MPI_DOUBLE, res, len, MPI_DOUBLE,
                Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, reslen);
  return Val_unit;
}

/* Reduce */

static MPI_Op reduce_intop[] =
  { MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_BAND, MPI_BOR, MPI_BXOR };
static MPI_Op reduce_floatop[] =
  { MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD };

value caml_mpi_reduce_int(value data, value op, value root, value comm)
{
  long d = Long_val(data);
  long r = 0;
  MPI_Reduce(&d, &r, 1, MPI_LONG,
             reduce_intop[Int_val(op)], Int_val(root), Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_reduce_intarray(value data, value result, value op,
                               value root, value comm)
{
  mlsize_t len = Wosize_val(data);
  int i, myrank;
  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  for (i = 0; i < len; i++) Field(data, i) = Long_val(Field(data, i));
  /* Do the reduce */
  MPI_Reduce(&Field(data, 0), &Field(result, 0), len, MPI_LONG,
             reduce_intop[Int_val(op)], Int_val(root), Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* At root node, also encode result */
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root)) caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_reduce_float(value data, value op, value root, value comm)
{
  double d = Double_val(data);
  double r = 0.0;
  MPI_Reduce(&d, &r, 1, MPI_DOUBLE,
             reduce_floatop[Int_val(op)], Int_val(root), Comm_val(comm));
  return copy_double(r);
}

value caml_mpi_reduce_floatarray(value data, value result, value op,
                            value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Reduce(d, res, len, MPI_DOUBLE,
             reduce_floatop[Int_val(op)], Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

/* Allreduce */

value caml_mpi_allreduce_int(value data, value op, value comm)
{
  long d = Long_val(data);
  long r;
  MPI_Allreduce(&d, &r, 1, MPI_LONG,
                reduce_intop[Int_val(op)], Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_allreduce_intarray(value data, value result, value op,
                                  value comm)
{
  mlsize_t len = Wosize_val(data);
  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  /* Do the reduce */
  MPI_Allreduce(&Field(data, 0), &Field(result, 0), len, MPI_LONG,
                reduce_intop[Int_val(op)], Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* Re-encode result at all nodes in place */
  caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_allreduce_float(value data, value op, value comm)
{
  double d = Double_val(data);
  double r;
  MPI_Allreduce(&d, &r, 1, MPI_DOUBLE,
                reduce_floatop[Int_val(op)], Comm_val(comm));
  return copy_double(r);
}

value caml_mpi_allreduce_floatarray(value data, value result, value op,
                                    value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Allreduce(d, res, len, MPI_DOUBLE,
                reduce_floatop[Int_val(op)], Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

/* Scan */

value caml_mpi_scan_int(value data, value op, value comm)
{
  long d = Long_val(data);
  long r;

  MPI_Scan(&d, &r, 1, MPI_LONG, reduce_intop[Int_val(op)], Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_scan_intarray(value data, value result, value op, value comm)
{
  mlsize_t len = Wosize_val(data);

  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  /* Do the scan */
  MPI_Scan(&Field(data, 0), &Field(result, 0), len, MPI_LONG,
           reduce_intop[Int_val(op)], Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* Encode result */
  caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_scan_float(value data, value op, value comm)
{
  double d = Double_val(data), r;

  MPI_Scan(&d, &r, 1, MPI_DOUBLE,
           reduce_floatop[Int_val(op)], Comm_val(comm));
  return copy_double(r);
}

value caml_mpi_scan_floatarray(value data, value result, value op, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Scan(d, res, len, MPI_DOUBLE,
           reduce_floatop[Int_val(op)], Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

