(** contains helper functions for various data structure *)

val shortest_path : string -> string -> Graph.vgt -> string list
(** [shortest_path start finish graph] is a list of the ids of the nodes along
    the shortest path from [start] to [finish], inclusive.
    Example: [ [start]; ...; [finish] ] *)

(* TODO: use "is" verb *)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a
(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Check repeats with
the continuing element and the third, etc. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)

val breadth_first : Graph.ugt -> int -> int list list list