(* represents an undirected graph *)

(** the graph abstract type *)
type gt

(** raised when a node with an unknown ID is referenced in a graph *)
exception UnknownNode of int

(** [empty] is a graph with no nodes *)
val empty : gt

(** [size graph] is the number of unique nodes contained in [graph] *)
val size : gt -> int

(** [ith i] returns the node at index [i]*)
val ith : gt -> int -> int list

(** [add_node graph connections id] creates node [id] with bidirectional connections
    to all [connections] in the [graph]. 
    Requires: 
    -[id] is not in [connections], 
    -[id] > 0
    -[id] has not been connected before (unless one wants to establish new connections)
    *)
val add_node : gt -> int -> int list -> gt

(** [connect_nodes graph id1 id2] is the modified graph of [gt] with an
    added connection between two existing nodes [id1] and [id2]. Raises
    UnknownNode if either node DNE in [gt] *) 
(**val connect_nodes : gt -> int -> int -> gt  [TEMPORARILY DISABLED]*)

(** [neighbors graph id] is the list of nodes in [graph] that [id] is
    connected to *)
val neighbors : gt -> int -> int list

(**[set graph] returns a set of all nodes contained by the graph *)
val set : gt -> int list
