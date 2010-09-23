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

/* $Id: msgs.c,v 1.6 2003/03/31 14:22:57 xleroy Exp $ */

/* Point-to-point communication */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "camlmpi.h"

extern void output_value_to_malloc(value v, value flags,
                                   /*out*/ char ** buf, /*out*/ long * len);
extern value input_value_from_malloc(char * data, long ofs);

/* Sending */

value caml_mpi_send(value data, value flags,
                    value dest, value tag, value vcomm)
{
  MPI_Comm comm = Comm_val(vcomm);
  char * buffer;
  long len;

  Begin_root(vcomm)             /* prevent deallocation of communicator */
    output_value_to_malloc(data, flags, &buffer, &len);
    /* This also allocates the buffer */
    enter_blocking_section();
    MPI_Send(buffer, len, MPI_BYTE, Int_val(dest), Int_val(tag), comm);
    leave_blocking_section();
  End_roots();
  stat_free(buffer);
  return Val_unit;
}

value caml_mpi_send_int(value data, value dest, value tag, value comm)
{
  long n = Long_val(data);
  MPI_Send(&n, 1, MPI_LONG, Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_send_intarray(value data, value dest, value tag, value comm)
{
  MPI_Send(&Field(data, 0), Wosize_val(data), MPI_LONG,
           Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_send_float(value data, value dest, value tag, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);

  MPI_Send(d, len, MPI_DOUBLE, Int_val(dest), Int_val(tag), Comm_val(comm));
  caml_mpi_free_floatarray(d);
  return Val_unit;
}

/* Probe for pending messages and determine length */

value caml_mpi_probe(value source, value tag, value comm)
{
  MPI_Status status;
  int count;
  value res;

  MPI_Probe(Int_val(source), Int_val(tag), Comm_val(comm), &status);
  MPI_Get_count(&status, MPI_BYTE, &count);
  res = alloc_tuple(3);
  Field(res, 0) = Val_int(count);
  Field(res, 1) = Val_int(status.MPI_SOURCE);
  Field(res, 2) = Val_int(status.MPI_TAG);
  return res;
}

/* Receive */

value caml_mpi_receive(value vlen, value source, value tag, value vcomm)
{
  MPI_Comm comm = Comm_val(vcomm);
  mlsize_t len = Long_val(vlen);
  char * buffer;
  MPI_Status status;
  value res;

  Begin_root(vcomm)             /* prevent deallocation of communicator */
    buffer = stat_alloc(len);
    enter_blocking_section();
    MPI_Recv(buffer, len, MPI_BYTE,
             Int_val(source), Int_val(tag), comm, &status);
    leave_blocking_section();
    res = input_value_from_malloc(buffer, 0);
    /* This also deallocates the buffer */
  End_roots();
  return res;
}

value caml_mpi_receive_int(value source, value tag, value comm)
{
  MPI_Status status;
  long n;

  MPI_Recv(&n, 1, MPI_LONG,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_long(n);
}

value caml_mpi_receive_intarray(value data, value source, value tag, value comm)
{
  MPI_Status status;

  MPI_Recv(&Field(data, 0), Wosize_val(data), MPI_LONG,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_unit;
}

value caml_mpi_receive_float(value source, value tag, value comm)
{
  MPI_Status status;
  double d;

  MPI_Recv(&d, 1 , MPI_DOUBLE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return copy_double(d);
}

value caml_mpi_receive_floatarray(value data, value source, value tag, value comm)
{
  MPI_Status status;
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_output_floatarray(data, len);

  MPI_Recv(d, len, MPI_DOUBLE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  caml_mpi_commit_floatarray(d, data, len);
  return Val_unit;
}

/* Auxiliaries */

value caml_mpi_get_any_tag(value unit)
{
  return Val_int(MPI_ANY_TAG);
}

value caml_mpi_get_any_source(value unit)
{
  return Val_int(MPI_ANY_SOURCE);
}


