(** contains helper functions for various data structures *)

val remove_all : 'a list -> 'a list -> 'a list
(** [remove_all list1 list2] is [list1] without the elements of [list2]*)

val relate_option : ('a -> 'a -> bool) -> 'a list -> 'a option
(** [relate f list] is the element in [list] which survives a relation
    chain of [f]. Begins by comparing the first two values [f e1 e2]. If
    [f e1 e2] then list --> [e1, e3...] else [e2, e3...] and this
    process recurses until only one element remains. *)

val breadth_first :
  Graph.vgt -> int -> int -> (int -> int -> float) -> int list
(** [breadth_first graph start_id end_id distance_f] is the list of ids
    of nodes of the shortest path from [start_id] to [end_id], with
    distance determined by distance function [distance_f]*)
