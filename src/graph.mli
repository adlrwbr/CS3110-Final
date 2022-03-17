type ugt
(** the unverified graph abstract type. Represents a bidirectional graph
    which may contain islands *)

type vgt
(** the verified graph abstract type. Guarenteed to not contain islands *)

exception UnknownNode of int

exception InvalidGraph
(** identifies an unverified graph that cannot be verified *)

val empty : ugt
(** [empty] is a graph with no nodes *)

(** [size graph] is the number of unique nodes contained in [graph] *)
val size : ugt -> int

val add : ugt -> int * ugt
(** [add graph] is the tuple ([id], [newgraph]) where [newgraph] is
    [graph] w/ the newly created node [id] *)

val add_no_id : ugt -> ugt
(** [add_no_id graph] is the [graph] w/ the newly created node [id] *)

val connect : int -> int -> ugt -> ugt
(** [connect id1 id2 graph] is a modified unverified graph w/ an additional
    edge b/w nodes [id1] and [id2]. Requires: [id1] != [id2] Raises:
    [UnknownNode id] if either [id1] or [id2] DNE within the graph *)

val unverify : vgt -> ugt
(** [unverify graph] converts the verified graph into one that is
no longer guaranteed to be  *)

val verify : ugt -> vgt
(** [verify graph] is the verified graph. Raises InvalidGraph if [graph]
    is not verifiable. *)

val neighbors : ugt -> int -> int list
(** [neighbors graph id] is the list of nodes in [graph] to which [id]
    is connected *)

val set : ugt -> int list
(** [set graph] is a set of all nodes contained by the graph *)
