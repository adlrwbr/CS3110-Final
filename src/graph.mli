(* represents an undirected graph *)

(** the unverified graph abstract type. Represents a bidirectional graph
    which may contain islands *)
type ugt

(** the verified graph abstract type. Guarenteed to not contain islands *)
type vgt

exception UnknownNode of int

(** identifies an unverified graph that cannot be verified *)
exception InvalidGraph

(** [empty] is a graph with no nodes *)
val empty : ugt

(** [add graph] is the id of a newly created node *)
val add : ugt -> int

(** [connect id1 id2] is a modified unverified graph w/ an additional edge b/w
    nodes [id1] and [id2].
    Requires: [id1] != [id2]
    Raises: [UnknownNode id] if either [id1] or [id2] DNE within the graph *)
val connect : ugt -> int -> int -> ugt

(** [verify graph] is the verified graph. Raises InvalidGraph if [graph] is
    not verifiable. *)
val verify : ugt -> vgt

(** [size graph] is the number of unique nodes contained in [graph] *)
val size : 'a -> int

(** [neighbors graph id] is the list of nodes in [graph] to which [id] is
    connected *)
val neighbors : 'a -> int -> int list

(** [set graph] is a set of all nodes contained by the graph *)
val set : 'a -> int list
