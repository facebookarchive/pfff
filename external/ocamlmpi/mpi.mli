(***********************************************************************)
(*                                                                     *)
(*                         The Caml/MPI interface                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id: mpi.mli,v 1.9 2003/03/31 14:22:57 xleroy Exp $ *)

(* Caml bindings for the Message Passing Interface (MPI) library *)

(*** Error reporting *)

exception Error of string
        (* Raised when an operation of the [Mpi] module encounters an error.
           The string argument describes the error. *)

(*** Basic operations on communicators *)

type communicator
        (* The type of communicators.  Communicators are groups of
           nodes (processing elements) that can exchange data. *)
type rank = int
        (* The type of ranks of nodes.  Nodes in a given communicator
           are assigned integer ranks 0, 1, \ldots, $N-1$ where $N$
           is the size of the communicator. *)
val comm_world: communicator
        (* The global communicator. *)
external comm_size: communicator -> int = "caml_mpi_comm_size"
        (* Return the size (number of nodes) in the given communicator. *)
external comm_rank: communicator -> rank = "caml_mpi_comm_rank"
        (* Return the rank of the calling node in the given communicator.
           The rank [Mpi.comm_rank c] is between 0 (inclusive) and
           [Mpi.comm_size c] (exclusive). *)

(*** Point-to-point communication *)

type tag = int
        (* The type of tags associated with messages in point-to-point
           communications.  Tags are positive integers in the range
           [0 .. 32767].  *)

val send: 'a -> rank -> tag -> communicator -> unit
        (* [Mpi.send d dst tag comm] sends a message containing data [d]
           to the node that has rank [dst] in communicator [comm].
           The message is sent with tag [tag].  Depending on the
           underlying MPI implementation, message sending can be
           synchronous or asynchronous; that is, [Mpi.send] can block
           until the target node receives the message, or [Mpi.send]
           can return before the target node has received the message. *)
val receive: rank -> tag -> communicator -> 'a
        (* [Mpi.receive src tag comm] blocks until a message is available,
           and returns the data contained in that message.
           The [src] argument selects the desired source for the message:
           if [src] is [Mpi.any_source], messages from any node in communicator
           [comm] are accepted; otherwise, only messages sent by the node
           having rank [src] in [comm] are accepted.
           Similarly, the [tag] argument selects messages by their tag:
           if [tag] is [Mpi.any_tag], messages are accepted regardless of
           their tags; otherwise, only messages with tag equal to [tag]
           are accepted.

           Warning: just like the [Marshal.from_*] functions,
           [Mpi.receive] is not type-safe.  The Caml value returned by
           [Mpi.receive]  does not possess type ['a]
           for all ['a]; it has one, unique type which cannot be determined
           at compile-type.  The programmer should be careful about using
           the returned value with the right type. *)
val receive_status: rank -> tag -> communicator -> 'a * rank * tag
        (* Same as [Mpi.receive], but returns a triple [(d, src, tag)]
           where [d] is the data associated with the message,
           [src] the rank of the node that sent the message,
           and [tag] the actual tag attached to the message. *)
val probe: rank -> tag -> communicator -> rank * tag
        (* [Mpi.probe src tag comm] blocks until a message is available
           on communicator [comm], with source and tag matching the
           [src] and [tag] arguments as described in [Mpi.receive].
           It then returns the rank of the node that sent the message
           and the actual tag attached to the message.  The message itself
           is not read, and can be retrieved later with [Mpi.receive]
           or [Mpi.receive_status]. *)
val any_tag: tag
val any_source: rank
        (* The special values of the [tag] and [src] arguments of
           [Mpi.receive], [Mpi.receive_status] and [Mpi.probe],
           indicating that any message tag is acceptable (for [Mpi.any_tag])
           or any message source is acceptable (for [Mpi.any_source]). *)

val send_int: int -> rank -> tag -> communicator -> unit
val receive_int: rank -> tag -> communicator -> int
val send_float: float -> rank -> tag -> communicator -> unit
val receive_float: rank -> tag -> communicator -> float
val send_int_array: int array -> rank -> tag -> communicator -> unit
val receive_int_array: int array -> rank -> tag -> communicator -> unit
val send_float_array: float array -> rank -> tag -> communicator -> unit
val receive_float_array: float array -> rank -> tag -> communicator -> unit
        (* Specialized versions of [Mpi.send] and [Mpi.receive]
           for communicating integers, floating-point numbers,
           arrays of integers, and arrays of floating-point numbers.
           These specialized versions are more efficient than
           [Mpi.send] and [Mpi.receive] since less copying is involved.
           The arguments to the [Mpi.send_*] functions have the same
           meaning as for [Mpi.send].
           The arguments to [Mpi.receive_int] and [Mpi.receive_float]
           have the same meaning as for [Mpi.receive].
           [Mpi.receive_int_array] and [Mpi.receive_float_array]
           have one extra argument, which is the array in which the data
           of the received message is stored.  The caller is responsible
           for pre-allocating an array large enough to hold the incoming data.

           It is an error to send a message using one of the specialized
           [Mpi.send_*] functions and receive it with the generic
           [Mpi.receive] function, and conversely. *)

(*** Group communication *)

val barrier: communicator -> unit
        (* [Mpi.barrier comm] suspends the calling process until all
           nodes in communicator [comm] are executing [Mpi.barrier comm].
           Then all nodes return from [Mpi.barrier] and continue executing. *)

(** Broadcast *)

val broadcast: 'a -> rank -> communicator -> 'a
        (* [Mpi.broadcast d root comm] broadcasts data [d] from node
           with rank [root] in [comm] to all other nodes in [comm].
           All nodes in [comm] must call [Mpi.broadcast] with the same
           [root] and [comm] arguments.  The [d] argument is significant
           only at node [root]; it is ignored at other nodes.
           [Mpi.broadcast] returns the broadcast data. *)
val broadcast_opt: 'a option -> rank -> communicator -> 'a
        (* Same as [Mpi.broadcast], except that the data (first argument)
           is provided as an option type.  The root node must provide a
           first argument of the form [Some d] where [d] is the data to
           broadcast.  The other node provide [None] as their first
           argument. *)
val broadcast_int: int -> rank -> communicator -> int
val broadcast_float: float -> rank -> communicator -> float
val broadcast_int_array: int array -> rank -> communicator -> unit
val broadcast_float_array: float array -> rank -> communicator -> unit
        (* Specialized versions of [Mpi.broadcast] for integers, floats,
           arrays of integers and arrays of floats.  For
           [Mpi.broadcast_int] and [Mpi.broadcast_float], the broadcast
           value is returned as result, and the first argument is significant
           only at the root node.
           For [Mpi.broadcast_int_array] and [Mpi.broadcast_float_array],
           the broadcast value is stored in the array passed as first argument;
           thus, the first argument is significant at all nodes. *)

(** Scatter *)
val scatter: 'a array -> rank -> communicator -> 'a
        (* [Mpi.scatter a root comm] scatters the elements of array [a]
           from node [root] to all nodes in [comm].  The node with rank [i]
           in [comm] receives the element [a.(i)] and returns it as result
           of [Mpi.scatter].  The [a] argument is significant only at node
           [root]; an empty array [[||]] can be given as first argument
           at other nodes. *)
val scatter_int: int array -> rank -> communicator -> int
val scatter_float: float array -> rank -> communicator -> float
        (* Specialized versions of [Mpi.scatter] for integers and floats. *)
val scatter_int_array: int array -> int array -> rank -> communicator -> unit
val scatter_float_array:
  float array -> float array -> rank -> communicator -> unit
        (* Specialized versions of [Mpi.scatter] for arrays of integers and
           arrays of floats.  [Mpi.scatter_int_array src dst root comm]
           splits the array [src] at node [root] into [Mpi.comm_size comm]
           chunks of size [Array.length dst], and sends the chunks to
           each node, storing them into array [dst] at each node.
           The [src] argument is significant only at node [root].
           [Mpi.scatter_int_array] is similar. *)

(** Gather *)
val gather: 'a -> rank -> communicator -> 'a array
        (* [Mpi.gather d root comm] gathers the values of the [d] argument
           at all nodes onto node [root], and returns those values as an
           array.  At node [root], [Mpi.gather] returns an array of
           size [Mpi.comm_size comm]; element number [i] is the value 
           provided for argument [d] by node [i].  At other nodes,
           the empty array [[||]] is returned. *)
val gather_int: int -> int array -> rank -> communicator -> unit
val gather_float: float -> float array -> rank -> communicator -> unit
        (* Specialized versions of [Mpi.gather] for integers and floats. *)
val gather_int_array: int array -> int array -> rank -> communicator -> unit
val gather_float_array:
  float array -> float array -> rank -> communicator -> unit
        (* Specialized versions of [Mpi.gather] for arrays of integers and
           arrays of floats.  [Mpi.gather_int_array src dst root comm]
           sends the arrays [src] at each node to the node [root].
           At node [root], the arrays are concatenated and stored in the
           argument [dst].  [dst] is significant only at node [root].
           [Mpi.gather_int_array] is similar. *)

(** Gather to all *)
val allgather: 'a -> communicator -> 'a array
val allgather_int: int -> int array -> communicator -> unit
val allgather_float: float -> float array -> communicator -> unit
val allgather_int_array: int array -> int array -> communicator -> unit
val allgather_float_array:
  float array -> float array -> communicator -> unit
        (* The [Mpi.allgather*] functions behave like the corresponding
           [Mpi.gather*] functions, except that the result of the gather
           operation is available at all nodes, not only at the root node.
           In other terms, [Mpi.allgather] is equivalent to [Mpi.gather]
           at root [r] followed by a broadcast of the result from node [r]. *)

(** Reduce *)
type intop =
  Int_max | Int_min | Int_sum | Int_prod | Int_land | Int_lor | Int_xor
type floatop =
  Float_max | Float_min | Float_sum | Float_prod
        (* The operations that can be performed by a reduce or scan,
           on integers and floats respectively. [max] and [min]
           are maximum and minimum; [sum] and [prod]
           are summation ([+]) and product ([*]).
           [land]. [lor] and [lxor] are logical (bit-per-bit) and,
           or and exclusive-or. *)

val reduce_int: int -> intop -> rank -> communicator -> int
val reduce_float: float -> floatop -> rank -> communicator -> float
        (* [Mpi.reduce_int d op root comm] computes the value of
           [d0 op d1 op ... op dN], where [d0 ... dN] are the values of
           the [d] argument at every node in [comm].  The result value
           is returned at node with rank [root].  A meaningless integer
           is returned at other nodes.  [Mpi.reduce_float] is similar
           except for the use of floating-point operations instead of
           integer operations. *)
val reduce_int_array:
  int array -> int array -> intop -> rank -> communicator -> unit
val reduce_float_array:
  float array -> float array -> floatop -> rank -> communicator -> unit
        (* [Mpi.reduce_int_array d res op root comm] computes 
           [Array.length d] reductions by operation [op] simultaneously.
           For every [i], the values of [d.(i)] at every node
           are combined using [op] and the result is stored into [dst.(i)]
           at node [root]. *)

(** Reduce to all *)
val allreduce_int: int -> intop -> communicator -> int
val allreduce_float: float -> floatop -> communicator -> float
val allreduce_int_array:
  int array -> int array -> intop -> communicator -> unit
val allreduce_float_array:
  float array -> float array -> floatop -> communicator -> unit
        (* The [Mpi.allreduce_*] operations are similar to the
           corresponding [Mpi.reduce_*] operations, except that the result
           of the reduction is made available at all nodes. *)

(** Scan *)
val scan_int: int -> intop -> communicator -> int
val scan_float: float -> floatop -> communicator -> float
        (* [Mpi.scan_int d res op comm] performs a scan operation over
           the integers [d] at every node.  Let [d0 ... dN] be the
           values of the [d] at every node in [comm].  At node with rank [R],
           [Mpi.scan_int d res op comm] returns [d0 op ... op dR].
           [Mpi.scan_float] is similar. *)
val scan_int_array: int array -> int array -> intop -> communicator -> unit
val scan_float_array:
  float array -> float array -> floatop -> communicator -> unit
        (* Same as [Mpi.scan_int] and [Mpi.scan_float], but perform several
           scanning operations on the elements of the input array (first
           argument).  The result is stored in the array passed as second
           argument at the root node. *)

(*** Advanced operations on communicators *)

val comm_compare: communicator -> communicator -> bool
        (* Compare two communicators and return [true] if they are the same,
           [false] otherwise. *)
        
type color = int
val comm_split: communicator -> color -> int -> communicator
        (* [Mpi.comm_split comm col key] splits the communicator into
           several communicators based on the values of [col] and
           [key] at every node.  For each distinct value of the [col]
           argument, a new communicator is created.  It contains all
           nodes of [comm] that have presented that particular value of
           [key] to [Mpi.comm_split].  The ordering of nodes in the
           new communicator is determined by the [key] argument:
           nodes are ordered by increasing values of [key], and in case
           of ties, by their original order in [comm].  Thus, to preserve
           the same ordering as in [comm], it suffices that all nodes
           present [0] as the [key] argument.  In each node, the communicator
           returned is the one that corresponds to the [color] argument
           of that node. *)
           
val color_none: color
        (* In [Mpi.comm_split], a node can pass [Mpi.color_none] as the
           [col] argument to indicate that it does not want to be part
           of any of the new communicators.  [Mpi.comm_split] then
           returns a null communicator (allowing no communications) in
           that node. *)

(** Cartesian topologies *)
val cart_create:
    communicator -> int array -> bool array -> bool -> communicator
        (* [Mpi.cart_create comm dims periodic reorder] embeds a cartesian
           topology (multi-dimensional grid) on the nodes of
           communicator [comm], and return a
           new communicator with that information attached.
           The length of [dims] determines the number of dimensions of
           the topology.  For each dimension [d], [dims.(d)] specifies
           the number of nodes in that dimension, and [periodic.(d)]
           says whether that dimension is periodic (wraps around) or not.
           [reorder] determines whether the ranks of nodes in the new
           communicator can be reordered for better efficiency ([true])
           or must remain the same as in [comm] ([false]).
           The initial communicator [comm] must contain at least as many
           nodes as specified by [dims]. *)
val dims_create: int -> int array -> int array
        (* [Mpi.dims_create numnodes hints] helps determining a
           suitable [dims] argument to [Mpi.cart_create]
           given a number of nodes [numnodes], the number of
           dimensions required, and optional constraints.
           The length of the [hints] array determines the number of
           dimensions.  For each dimension [d], [hints.(d)], if not null,
           is the number of nodes required along this dimension.  If null,
           [Mpi.dims_create] figures out a suitable number.

           For instance, [Mpi.dims_create 24 [|0;0|]] returns reasonable
           dimensions for a two-dimensional grid containing 24 nodes. *)

val cart_rank: communicator -> int array -> rank
        (* [Mpi.cart_rank comm coords] return the rank of the node in
           the cartesian topology [comm] that is at coordinates [coords].
           The [coords] array must have one element per dimension of the
           cartesian topology.  Individual coordinates range between [0]
           (inclusive) and the corresponding dimension (exclusive). *)
val cart_coords: communicator -> rank -> int array
        (* The inverse operation of [Mpi.cart_rank].
           [Mpi.cart_coords comm r] returns the cartesian coordinates
           of the node having rank [r] in [comm]. *)

(*** Process group management *)

type group
        (* The type of groups.  Groups represent sets of nodes
           (processing elements).  Unlike communicators, they cannot
           be used directly for communication.  Instead, one constructs
           a group representing the desired set of nodes, then build
           a communicator for this group.  *)

val comm_create: communicator -> group -> communicator
        (* [Mpi.comm_create comm group] creates a communicator
           whose nodes are those described in [group].  [comm] is
           the initial communicator; the nodes in [group] must be
           a subset of those in [comm].  The null communicator is
           returned to the nodes that are not part of [group]. *)

val group_size: group -> int
        (* Return the size (number of nodes) in the given group. *)
val group_rank: group -> rank
        (* Return the rank of the calling node in the given group. *)

val group_translate_ranks: group -> rank array -> group -> rank array
        (* [Mpi.group_translate_ranks g1 ranks g2] translates the ranks
           of a number of nodes from one group to another.  [rank]
           is an array of node ranks relative to group [g1].  The
           returned array contains the ranks for the same nodes, relative
           to group [g2]. *)

val comm_group: communicator -> group
        (* [Mpi.comm_group comm] returns the group of all nodes belonging
           to the communicator [comm], with the same ranks as in [comm]. *)
val group_union: group -> group -> group
val group_intersection: group -> group -> group
val group_difference: group -> group -> group
        (* Union, intersection and set difference over groups. *)

val group_incl: group -> rank array -> group
        (* [Mpi.group_incl group ranks] returns the subset of [group]
           containing the nodes whose ranks are given in the array [ranks]. *)
val group_excl: group -> rank array -> group
        (* [Mpi.group_excl group ranks] returns the subset of [group]
           containing the nodes whose ranks are not given in the array
           [ranks]. *)

type group_range = { range_first: int; range_last: int; range_stride: int }
        (* A group range represents the set of nodes whose ranks are
           ([range_first]; [range_first + range_stride]; ...; [range_last]). *)

val group_range_incl: group -> group_range array -> group
        (* [Mpi.group_range_incl group ranges] returns the subset of [group]
           containing the nodes whose ranks belong to the ranges
           listed in [ranges]. *)
val group_range_excl: group -> group_range array -> group
        (* [Mpi.group_range_excl group ranges] returns the subset of [group]
           containing the nodes whose ranks do not belong to the ranges
           listed in [ranges]. *)

(*** Miscellaneous *)

external wtime: unit -> float = "caml_mpi_wtime"
        (* Return the wall-clock time elapsed at the calling node
           since the beginning of the program execution. *)

