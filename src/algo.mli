val remove_all : 'a list -> 'a list -> 'a list
(** [remove_all list1 list2] removes all elements [list2] from [list1] *)

(** [tl list] returns the last element in the list *)
val tl : 'a list -> 'a

(** [relate f list] traverses [list] comparing the first two elements with [f].
If [f] is true, first continues, else the second continues. Finishes once there is
one element that has survived the comparison chain. 
Requires: [list] contains at least one element.*)
val relate : ('a -> 'a -> bool) -> 'a list -> 'a

(* TODO: fix this spec and compilation error *)
(* val bfs : Graph.vgt -> int list -> int list *)
(** [bfs ids] Begins a breadth first search *)
