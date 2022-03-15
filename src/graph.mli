(* represents an undirected graph *)

type gt
(** the graph abstract type *)

exception UnknownNode of int
(** raised when a node with an unknown ID is referenced in a graph *)

val empty : gt
(** [empty] is a graph with no nodes *)

val size : gt -> int
(** [size graph] is the number of unique nodes contained in [graph] *)

val add_node : gt -> int -> int list -> gt
(** [add_node graph connections id] creates node [id] with bidirectional connections
    to all [connections] in the [graph]. 
    Requires: 
    [id] is not in [connections], [id] > 0
    [id] has not been previously added. *)

(**val connect_nodes : gt -> int -> int -> gt  [TEMPORARILY DISABLED]*)
(** [connect_nodes graph id1 id2] is the modified graph of [gt] with an
    added connection between two existing nodes [id1] and [id2]. Raises
    UnknownNode if either node DNE in [gt] *) 

val neighbors : gt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] that [id] is
    connected to *)
