(** contains helper functions for various data structure *)

val slope : float -> float -> float -> float -> float
(** [slope x1 y1 x2 y2] is the slope between two coordinate pairs (x1, y1) and
    (x2, y2). *)

val distance : (float * float) -> (float * float) -> float
(** [distance p1 p2] is the distance between coordinate pairs [p1] and [p2] *)

val in_range : float -> float -> float -> bool
(** [in_range p p1 p2] is whether or not [p] is between [p1] and [p2],
    inclusive. *)

val shortest_path : int -> int -> Graph.vgt -> int list
(** [shortest_path start finish graph] is a list of the ids of the nodes along
    the shortest path from [start] to [finish], inclusive.
    Example: [ [start]; ...; [finish] ] *)

(* TODO - Sean: use "is" verb *)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a
(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Check repeats with
the continuing element and the third, etc. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)

val breadth_first : Graph.vgt -> int -> int list list list
(** [breadth_first graph start_id] is the breadth-first traversal from the node
[start_id]. The first tier of the list contains the nodes n-connections away from
the root node. The second tier of the list contains the connections from the previous
node at that index. The third tier is the individual connections from that previous node.
Example: (let --> mean connected to) 2-->3,5, 3-->1,4, breadth_first of this graph
starting at 2 would be [[[2]];[[3;5]];[[1;4];[]]].
*)
