(**
   Generalized suffix trees (GSTs).

   Computes generalized suffix trees from list of strings. A terminal symbol is
   implicitly added to them, but is not returned in the word labeling nodes and
   leaves. This should allow a rather transparent handling of GSTs.

   Node-based accesses are provided (sequences, root, children, suffix links,
   node labels, index), as well as a functional for synthesizing attributes from
   a GST. A readable representation of GSTs is derived from the later.
*)
(* made by Sebastien Ferre *)

type node
      (** Type of nodes in GSTs. *)

type t
      (** Type of GSTs. *)

val make : string list -> t
    (** [make l_str] computes a GST based on the set of strings given in [l_str]. *)

val string_list : t -> string list
    (** [string_list gst] returns the list of strings from which [gst] was computed. *)

val string : t -> int -> string
    (** [string gst k] returns the sequence number [k] (starting from 0). *)

val root : t -> node
    (** [root gst] returns the root node of the gst. *)

val word : t -> node -> string
    (** [word gst n] returns the word labeling node [n] in [gst]. *)

val children : t -> node -> node list
    (** [children gst n] returns a list of the children nodes of [n] in [gst]. *)

val linked_node : t -> node -> node
    (** [linked_node gst n] returns the node pointed by the suffix link from [n] in [gst]. *)

val index : t -> node -> int * int
    (** [index gst n] returns the index of a leaf [n] in [gst].
          This index is a pair [(k,i)], where [k] is the number of the sequence (as used by [string]), and
          [i] is the position of the related suffix (starting from [0] as usual in strings).
       @raise Invalid_argument "Suffix_tree.index: not a leaf" if [n] is not a leaf (has some child). *)

val implicit_node : t -> string -> node * string * node
    (** [implicit_node gst word] returns an implicit_node [(node,word',child)], where [node] is the lowest
       node in the suffix tre such that the concatenation of the word recognized by [node] and [word'] is
       equal to [word], if [word'] is not the empty string, then [child] is the child node of [node], whose
       label has [word'] as a prefix.
       @raise Not_found when [word] is not a substring of [string_list gst]. *)


val fold : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> 's
    (** [fold gst filter herit synth init] computes some attribute(s) over a GST by using the 3 functions
       [filter], [herit], [synth], and the initial value [init] inherited by the root node. ['h] is the type
       of inherited attributes, and ['s] is the type of synthesized attributes, and so the type of the result.

       The meaning of 3 functions is as follows:
       - [filter h child] returns [true] if the node [child] must be explored given the inherited value of the current
         node (parent of [child]),
       - [herit h child] returns the value inherited by [child] given the inherited value of the current node
         (parent of [child]),
       - [synth l h node] returns the synthesized value of the current node, given its inherited value [h], and
         the list [l] of synthesized values of explored children of [node] (according to [filter]).

     *)

val fold_node : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> node -> 's
    (** Same as [fold], except the computation starts and finishes at the last argument node. *)

val fold_s : t -> ('s list -> node -> 's) -> 's
    (** [fold_s gst synth] is equivalent to [fold gst filter herit synth init], where there is no filtering, and 
       no inherited values: purely synthetic. *)

val fold_s_node : t -> ('s list -> node -> 's) -> node -> 's
    (** Same as [fold_s], except the computation starts and finishes at the last argument node. *)

val fold_fs : t -> (node -> bool) -> ('s list -> node -> 's) -> 's
    (** [fold_fs gst filter synth] is equivalent to [fold gst filter herit synth init], where there is no inherited
       values. *)

type tree = Node of string * tree list | Leaf of string * (int * int)
val readable : t -> tree
    (** [readable gst] returns a (more) readable representation of [gst].
       Each node and leaf is decorated by its word label, and leaves are
       also decorated by their index. *)

val exact_matches : t -> string -> (int * int) list
