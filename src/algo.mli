val remove_all : 'a list -> 'a list -> 'a list
(** [remove_all list1 list2] removes all elements [list2] from [list1] *)

(** [last list] is the last element in the [list] 
    Requires: [list] is not empty. *)
val last : 'a list -> 'a

(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a

(** [bfs graph ids] is a set-like list of all nodes in [graph] sorted by number of edges
taken to reach id in [ids].*)
val bfs : Graph.ugt -> int -> int list
