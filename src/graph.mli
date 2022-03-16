(* represents an undirected graph *)
type gt

type ugt = gt
(** the unverified graph abstract type. Represents a bidirectional graph
    which may contain islands *)

type vgt = gt
(** the verified graph abstract type. Guarenteed to not contain islands *)

exception UnknownNode of int

exception InvalidGraph
(** identifies an unverified graph that cannot be verified *)

val empty : ugt
(** [empty] is a graph with no nodes *)

(** [size graph] is the number of unique nodes contained in [graph] *)
val size : gt -> int

val add : gt -> int * ugt
(** [add graph] is the tuple ([id], [newgraph]) where [newgraph] is
    [graph] w/ the newly created node [id] *)

val add_no_id : gt -> ugt
(** [add_no_id graph] is the [graph] w/ the newly created node [id] *)

val connect : gt -> int -> int -> ugt
(** [connect id1 id2] is a modified unverified graph w/ an additional
    edge b/w nodes [id1] and [id2]. Requires: [id1] != [id2] Raises:
    [UnknownNode id] if either [id1] or [id2] DNE within the graph *)

val verify : ugt -> vgt
(** [verify graph] is the verified graph. Raises InvalidGraph if [graph]
    is not verifiable. *)

val neighbors : gt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] to which [id]
    is connected *)

val set : gt -> int list
(** [set graph] is a set of all nodes contained by the graph *)
