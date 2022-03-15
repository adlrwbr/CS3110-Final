(* represents an undirected graph *)
type gt 

(** the unverified graph abstract type. Represents a bidirectional graph
    which may contain islands *)
type ugt = gt

(** the verified graph abstract type. Guarenteed to not contain islands *)
type vgt = gt

exception UnknownNode of int

(** identifies an unverified graph that cannot be verified *)
exception InvalidGraph

(** [empty] is a graph with no nodes *)
val empty : ugt

(** [size graph] is the number of unique nodes contained in [graph] *)
val size : gt -> int

(** [add graph] is the tuple ([id], [newgraph]) where [newgraph] is [graph] w/
    the newly created node [id] *)
val add : gt -> int * ugt

(** [connect id1 id2] is a modified unverified graph w/ an additional edge b/w
    nodes [id1] and [id2].
    Requires: [id1] != [id2]
    Raises: [UnknownNode id] if either [id1] or [id2] DNE within the graph *)
val connect : gt -> int -> int -> ugt

(** [verify graph] is the verified graph. Raises InvalidGraph if [graph] is
    not verifiable. *)
val verify : gt -> vgt

(** [neighbors graph id] is the list of nodes in [graph] to which [id] is
    connected *)
val neighbors : gt -> int -> int list

(** [set graph] is a set of all nodes contained by the graph *)
val set : gt -> int list
