(** contains helper functions for various data structure *)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a
(** [relate f list] is the element in [list] which survives a relation chain of [f].
Begins by comparing the first two values [f e1 e2]. 
If [f e1 e2] then repeat on [e1, e3, e4...], otherwise [e2, e3, e4...] until
a single element remains. 
Requires: [list] has at least one element*)

val relate_option : ('a -> 'a -> bool) -> 'a list -> 'a option
(** [relate_option f list] is the same as [relate f list] except does not require
[list] has >0 elements *)

val slope : float -> float -> float -> float -> float
(** [slope x1 y1 x2 y2] is the slope between two coordinate pairs (x1, y1) and
    (x2, y2). *)

val distance : (float * float) -> (float * float) -> float
(** [distance p1 p2] is the distance between coordinate pairs [p1] and [p2] *)

val in_range : float -> float -> float -> bool
(** [in_range p p1 p2] is whether or not [p] is between [p1] and [p2],
    inclusive. *)

val remove_all : 'a list -> 'a list -> 'a list
(** [remove_all list1 list2] is [list1] without the elements of [list2]*)

val shortest_path : int -> int -> Graph.vgt -> int list
(** [shortest_path start finish graph] is a list of the ids of the nodes along
    the shortest path from [start] to [finish], inclusive.
    Example: [ [start]; ...; [finish] ] *)

val breadth_first : Graph.vgt -> int -> int -> (int -> int -> float) -> int list
(** [breadth_first graph start_id] is the breadth-first traversal from the node
[start_id]. The first tier of the list contains the nodes n-connections away from
the root node. The second tier of the list contains the connections from the previous
node at that index. The third tier is the individual connections from that previous node.
Example: (let --> mean connected to) 2-->3,5, 3-->1,4, breadth_first of this graph
starting at 2 would be [[[2]];[[3;5]];[[1;4];[]]].
*)

val distance_between : Graph.vgt -> int -> int -> (int -> int -> float) -> float
(** [distance_between graph id1 id2 distance_f] is the shortest* distance between [id1] and [id2] where
edge weights are assigned by [distance_f id id]. *)

