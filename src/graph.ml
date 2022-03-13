(* represents an undirected graph *)

type gt = int list list
(** the graph abstract type *)

exception UnknownNode of int
(** raised when a node with an unknown ID is referenced in a graph *)

(** [empty] is a graph with no nodes *)
let empty = [ [] ]

(** [size graph] is the number of unique nodes contained in [graph] *)
let size graph = List.length graph

(** [add_node graph id_list] is a tuple containing the modified [graph]
    with an additional node and that new node’s id *)
let add_node graph id_list = ([], 2)

(** [connect_nodes graph id1 id2] is the modified graph of [gt] with an
    added connection between two existing nodes [id1] and [id2]. Raises
    UnknownNode if either node DNE in [gt] *)
let connect_nodes graph id1 id2 = [ [] ]

(** [neighbors graph id] is the list of nodes in [graph] that [id] is
    connected to *)
let neighbors graph id = []
