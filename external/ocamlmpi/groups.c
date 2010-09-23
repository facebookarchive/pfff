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

/* $Id: groups.c,v 1.2 2003/03/31 14:38:37 xleroy Exp $ */

/* Handling of groups */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "camlmpi.h"

static void caml_mpi_finalize_group(value v)
{
  MPI_Group_free(&Group_val(v));
}

value caml_mpi_alloc_group(MPI_Group g)
{
  value res =
    alloc_final(1 + (sizeof(MPI_Group) + sizeof(value) - 1) / sizeof(value),
                caml_mpi_finalize_group, 1, 100);
  Group_val(res) = g;
  return res;
}

value caml_mpi_group_size(value group)
{
  int size;
  MPI_Group_size(Group_val(group), &size);
  return Val_int(size);
}

value caml_mpi_group_rank(value group)
{
  int size;
  MPI_Group_rank(Group_val(group), &size);
  return Val_int(size);
}

value caml_mpi_group_translate_ranks(value group1, value ranks, value group2)
{
  int n = Wosize_val(ranks);
  int * ranks1 = stat_alloc(n * sizeof(int));
  int * ranks2 = stat_alloc(n * sizeof(int));
  int i;
  value res;

  for (i = 0; i < n; i++) ranks1[i] = Int_val(Field(ranks, i));
  MPI_Group_translate_ranks(Group_val(group1), n, ranks1,
                            Group_val(group2), ranks2);
  res = alloc(n, 0);
  for (i = 0; i < n; i++) Field(res, i) = Val_int(ranks2[i]);
  stat_free(ranks1);
  stat_free(ranks2);
  return res;
}

value caml_mpi_comm_group(value comm)
{
  MPI_Group group;
  MPI_Comm_group(Comm_val(comm), &group);
  return caml_mpi_alloc_group(group);
}

value caml_mpi_group_union(value group1, value group2)
{
  MPI_Group group;
  MPI_Group_union(Group_val(group1), Group_val(group2), &group);
  return caml_mpi_alloc_group(group);
}

value caml_mpi_group_difference(value group1, value group2)
{
  MPI_Group group;
  MPI_Group_difference(Group_val(group1), Group_val(group2), &group);
  return caml_mpi_alloc_group(group);
}

value caml_mpi_group_intersection(value group1, value group2)
{
  MPI_Group group;
  MPI_Group_intersection(Group_val(group1), Group_val(group2), &group);
  return caml_mpi_alloc_group(group);
}

value caml_mpi_group_incl(value group, value vranks)
{
  MPI_Group newgroup;
  int n = Wosize_val(vranks);
  int * ranks = stat_alloc(n * sizeof(int));
  int i;

  for (i = 0; i < n; i++) ranks[i] = Int_val(Field(vranks, i));
  MPI_Group_incl(Group_val(group), n, ranks, &newgroup);
  stat_free(ranks);
  return caml_mpi_alloc_group(newgroup);
}

value caml_mpi_group_excl(value group, value vranks)
{
  MPI_Group newgroup;
  int n = Wosize_val(vranks);
  int * ranks = stat_alloc(n * sizeof(int));
  int i;

  for (i = 0; i < n; i++) ranks[i] = Int_val(Field(vranks, i));
  MPI_Group_excl(Group_val(group), n, ranks, &newgroup);
  stat_free(ranks);
  return caml_mpi_alloc_group(newgroup);
}

static void caml_mpi_extract_ranges(value vranges,
                                    /*out*/ int * num,
                                    /*out*/ int (**rng)[3])
{
  int n = Wosize_val(vranges);
  int (*ranges)[3] = stat_alloc(n * sizeof(int[3]));
  int i;
  for (i = 0; i < n; i++) {
    value rng = Field(vranges, i);
    ranges[n][0] = Int_val(Field(rng, 0));
    ranges[n][1] = Int_val(Field(rng, 1));
    ranges[n][2] = Int_val(Field(rng, 2));
  }
  *num = n;
  *rng = ranges;
}

value caml_mpi_group_range_incl(value group, value vranges)
{
  int num;
  int (*ranges)[3];
  MPI_Group newgroup;
  caml_mpi_extract_ranges(vranges, &num, &ranges);
  MPI_Group_range_incl(Group_val(group), num, ranges, &newgroup);
  stat_free(ranges);
  return caml_mpi_alloc_group(newgroup);
}

value caml_mpi_group_range_excl(value group, value vranges)
{
  int num;
  int (*ranges)[3];
  MPI_Group newgroup;
  caml_mpi_extract_ranges(vranges, &num, &ranges);
  MPI_Group_range_excl(Group_val(group), num, ranges, &newgroup);
  stat_free(ranges);
  return caml_mpi_alloc_group(newgroup);
}


