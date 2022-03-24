(** contains helper functions for various data structure *)

val remove_all : 'a list -> 'a list -> 'a list
(** [remove_all list1 list2] is [list1] without the elements of [list2]*)

val last : 'a list -> 'a
(** [last list] is the last element in the [list] 
    Requires: [list] is not empty. *)

(* TODO: use "is" verb *)
(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Check repeats with
the continuing element and the third, etc. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a

(** [bfs graph ids] is a set-like list of all nodes in [graph] sorted by number of edges
taken to reach id in [ids].*)
val bfs : Graph.ugt -> int -> int list

val shortest_path : string -> string -> Graph.vgt -> string list
(** [shortest_path start finish graph] is an inclusive list of the ids of the
    nodes along the shortest path from [start] to [finish], inclusive.
    Example: [ [start]; ...; [finish] ] *)
