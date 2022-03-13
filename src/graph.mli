(* represents an undirected graph *)

type gt
(** the graph abstract type *)

exception UnknownNode of int
(** raised when a node with an unknown ID is referenced in a graph *)

val empty : gt
(** [empty] is a graph with no nodes *)

val size : gt -> int
(** [size graph] is the number of unique nodes contained in [graph] *)

val add_node : gt -> int list -> gt * int
(** [add_node graph id_list] is a tuple containing the modified [graph]
    with an additional node and that new node’s id *)

val connect_nodes : gt -> int -> int -> gt
(** [connect_nodes graph id1 id2] is the modified graph of [gt] with an
    added connection between two existing nodes [id1] and [id2]. Raises
    UnknownNode if either node DNE in [gt] *)

val neighbors : gt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] that [id] is
    connected to *)
